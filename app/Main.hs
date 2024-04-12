{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Aeson
import Data.List ((\\))
import qualified Data.Map as M
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      fileContents <- decodeUtf8 @Text @ByteString <$> readFileBS filePath
      case parse solution filePath fileContents of
        Right result -> putLBSLn $ encode $ buildTree result
        Left _ -> exitFailure
    _ -> putStrLn "wrong number of arguments (expecting one)" *> exitFailure

-- parsing

data Header = Header
  { fileFormatVersion :: Version,
    visualStudioMajorVersion :: Int,
    visualStudioVersion :: Version,
    minimumVisualStudioVersion :: Version
  }
  deriving (Show, Eq)

type Version = [Int]

-- TODO: better representation?
type Guid = Text

type Path = [Text]

data Project = Project
  { projectType :: Guid,
    projectName :: Text,
    projectPath :: Path,
    projectId :: Guid
  }
  deriving (Show, Eq)

newtype Global = Global
  {nestedProjects :: [(Guid, Guid)]}
  deriving (Show, Eq, Semigroup, Monoid)

data Solution = Solution
  { solutionHeader :: Header,
    solutionProjects :: [Project],
    solutionGlobal :: Global
  }
  deriving (Show, Eq)

type Parser s u m a = (Stream s m Char) => ParsecT s u m a

data TreeStructure
  = LeafProject Text Path
  | Folder Text Path [TreeStructure]
  deriving (Show, Eq, Generic)

instance ToJSON TreeStructure where
  toEncoding (LeafProject n p) =
    pairs ("type" .= ("Project" :: Text) <> "name" .= n <> "path" .= p)
  toEncoding (Folder n p children) =
    pairs $
      "type" .= ("Folder" :: Text)
        <> "name" .= n
        <> "path" .= p
        <> "children" .= children

buildTree :: Solution -> [TreeStructure]
buildTree (Solution {solutionProjects, solutionGlobal}) =
  rootProjects & map build
  where
    build proj = case parentToChildren M.!? proj of
      Just children -> Folder projectName projectPath (map build children)
      Nothing -> LeafProject projectName projectPath
      where
        (Project {projectName, projectPath}) = projectsById M.! proj
    rootProjects = map projectId solutionProjects \\ M.keys childToParent
    parentToChildren = foldr f mempty (nestedProjects solutionGlobal)
    f (child, parent) = M.alter (Just . (child :) . fromMaybe []) parent
    childToParent =
      foldr (uncurry M.insert) mempty (nestedProjects solutionGlobal)
    projectsById = M.fromList $ map (projectId &&& id) solutionProjects

solution :: Parser s u m Solution
solution =
  Solution
    <$> (P.optional bom *> P.optional endOfLine *> header)
    <*> sepEndBy project endOfLine
    <*> global

bom :: Parser s u m ()
bom = void (char (chr 0xffef) <|> char (chr 0xfeff))

global :: Parser s u m Global
global = do
  void (string "Global" <* endOfLine)
  sections <- endBy globalSection endOfLine
  void $ string "EndGlobal"
  pure $ Global {nestedProjects = fromMaybe mempty $ mconcat sections}

globalSection :: Parser s u m (Maybe [(Guid, Guid)])
globalSection = do
  void (skipMany space *> string "GlobalSection")
  sectionType <- betweenBrackets (P.many (satisfy (/= ')')))
  preOrPost <-
    string " = "
      *> (try (string "preSolution") <|> string "postSolution")
      <* endOfLine
  case (sectionType, preOrPost) of
    ("NestedProjects", "preSolution") -> do
      Just
        <$> manyTill
          (skipMany space *> folderRelation <* endOfLine)
          endGlobalSection
    _ ->
      manyTill arbitraryLine endGlobalSection $> Just []
  where
    endGlobalSection =
      void $ try (skipMany space *> string "EndGlobalSection")

arbitraryLine :: Parser s u m ()
arbitraryLine = void $ manyTill anyToken endOfLine

folderRelation :: Parser s u m (Guid, Guid)
folderRelation =
  (,) <$> betweenBraces guid <*> (string " = " *> betweenBraces guid)

project :: Parser s u m Project
project = do
  void $ string "Project"
  projectType <- betweenBrackets $ betweenQuotes $ betweenBraces guid
  void $ string " = "
  projectName <- betweenQuotes (toText <$> P.many (satisfy (/= '"')))
  void $ string ", "
  projectPath <- betweenQuotes path
  void $ string ", "
  projectId <- betweenQuotes (betweenBraces guid) <* endOfLine
  void $ manyTill arbitraryLine (string "EndProject")
  pure $ Project {projectType, projectName, projectPath, projectId}

path :: (Stream s m Char) => ParsecT s u m Path
path = sepBy1 pathElement (char '/' <|> char '\\')
  where
    pathElement = fmap toText $ P.many $ noneOf "/\"\\"

betweenBrackets :: Parser s u m a -> Parser s u m a
betweenBrackets = between (char '(') (char ')')

betweenQuotes :: Parser s u m a -> Parser s u m a
betweenQuotes = between (char '"') (char '"')

betweenBraces :: Parser s u m a -> Parser s u m a
betweenBraces = between (char '{') (char '}')

guid :: Parser s u m Guid
guid =
  toText . intercalate "-"
    <$> sequence
      [ count 8 hexDigit,
        char '-' *> count 4 hexDigit,
        char '-' *> count 4 hexDigit,
        char '-' *> count 4 hexDigit,
        char '-' *> count 12 hexDigit
      ]

header :: Parser s u m Header
header =
  Header
    <$> (skipMany space *> fileFormatVersionP)
    <*> visualStudioMajorVersionP
    <*> visualStudioVersionP
    <*> minimumVisualStudioVersionP
  where
    fileFormatVersionP =
      string "Microsoft Visual Studio Solution File, Format Version "
        *> version
        <* endOfLine
    visualStudioMajorVersionP = majorVersionPrefix *> int <* endOfLine
    majorVersionPrefix =
      string "# Visual Studio " *> P.optional (string "Version ")
    visualStudioVersionP =
      string "VisualStudioVersion = " *> version <* endOfLine
    minimumVisualStudioVersionP =
      string "MinimumVisualStudioVersion = " *> version <* endOfLine

version :: Parser s u m Version
version = sepBy1 int (char '.')

-- TODO: better int parsing?
int :: Parser s u m Int
int = P.many digit >>= toInt
  where
    toInt = maybe (fail "failed parsing an int") pure . readMaybe . toString
