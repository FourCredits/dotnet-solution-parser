{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.Aeson
import Data.List ((\\))
import qualified Data.Map as M
import qualified Text.Parsec as P

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      fileContents <- readFileBS filePath
      let parseResult = P.parse solution filePath fileContents
      print parseResult
      whenRight_ parseResult $ \s -> do
        let tree = buildTree s
        print tree
        putLBSLn $ encode tree
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

type Parser s u m a = (P.Stream s m Char) => P.ParsecT s u m a

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
    <$> header
    <*> P.sepEndBy project P.newline
    <*> global

global :: Parser s u m Global
global = do
  void $ P.string "Global\n"
  sections <- P.endBy globalSection P.newline
  void $ P.string "EndGlobal"
  pure $ Global {nestedProjects = fromMaybe mempty $ mconcat sections}

globalSection :: Parser s u m (Maybe [(Guid, Guid)])
globalSection = do
  void $ ignoreStartingSpaces (P.string "GlobalSection")
  sectionType <- betweenBrackets (P.many (P.satisfy (/= ')')))
  preOrPost <-
    P.string " = "
      *> (P.try (P.string "preSolution") <|> P.string "postSolution")
      <* P.newline
  case (sectionType, preOrPost) of
    ("NestedProjects", "preSolution") -> do
      Just
        <$> P.manyTill
          (ignoreStartingSpaces folderRelation <* P.newline)
          endGlobalSection
    _ ->
      P.manyTill arbitraryLine endGlobalSection $> Just []
  where
    endGlobalSection =
      void $ P.try $ ignoreStartingSpaces $ P.string "EndGlobalSection"

arbitraryLine :: Parser s u m ()
arbitraryLine = void $ P.manyTill P.anyToken P.newline

folderRelation :: Parser s u m (Guid, Guid)
folderRelation =
  (,) <$> betweenBraces guid <*> (P.string " = " *> betweenBraces guid)

-- NOTE: assumes that the parser only uses one line
ignoreStartingSpaces :: Parser s u m a -> Parser s u m a
ignoreStartingSpaces p = P.skipMany (P.satisfy (`elem` whitespace)) *> p
  where
    whitespace = "\n\t" :: String

-- TODO: there is sometimes stuff inside a between the first line and
-- `EndProject`, right?
project :: Parser s u m Project
project = do
  void $ P.string "Project"
  projectType <- betweenBrackets $ betweenQuotes $ betweenBraces guid
  void $ P.string " = "
  projectName <- betweenQuotes (toText <$> P.many (P.satisfy (/= '"')))
  void $ P.string ", "
  projectPath <- betweenQuotes path
  void $ P.string ", "
  projectId <- betweenQuotes $ betweenBraces guid
  void $ P.string "\nEndProject"
  pure $ Project {projectType, projectName, projectPath, projectId}

path :: (P.Stream s m Char) => P.ParsecT s u m Path
path = P.sepBy1 pathElement (P.char '/' <|> P.char '\\')
  where
    pathElement = fmap toText $ P.many $ P.satisfy (`notElem` illegalPathChars)
    illegalPathChars = "/\"\\" :: String

betweenBrackets :: Parser s u m a -> Parser s u m a
betweenBrackets = P.between (P.char '(') (P.char ')')

betweenQuotes :: Parser s u m a -> Parser s u m a
betweenQuotes = P.between (P.char '"') (P.char '"')

betweenBraces :: Parser s u m a -> Parser s u m a
betweenBraces = P.between (P.char '{') (P.char '}')

guid :: Parser s u m Guid
guid =
  toText . intercalate "-"
    <$> sequence
      [ P.count 8 P.hexDigit,
        P.char '-' *> P.count 4 P.hexDigit,
        P.char '-' *> P.count 4 P.hexDigit,
        P.char '-' *> P.count 4 P.hexDigit,
        P.char '-' *> P.count 12 P.hexDigit
      ]

header :: Parser s u m Header
header =
  Header
    <$> fileFormatVersionP
    <*> visualStudioMajorVersionP
    <*> visualStudioVersionP
    <*> minimumVisualStudioVersionP
  where
    fileFormatVersionP =
      P.string "Microsoft Visual Studio Solution File, Format Version "
        *> version
        <* P.newline
    visualStudioMajorVersionP =
      P.string "# Visual Studio Version " *> int <* P.newline
    visualStudioVersionP =
      P.string "VisualStudioVersion = " *> version <* P.newline
    minimumVisualStudioVersionP =
      P.string "MinimumVisualStudioVersion = " *> version <* P.newline

version :: Parser s u m Version
version = P.sepBy1 int (P.char '.')

-- TODO: better int parsing?
int :: Parser s u m Int
int = P.many P.digit >>= toInt
  where
    toInt = maybe (fail "couldn't parse an integer") pure . readMaybe . toString
