{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Examples.FileSystem where
import Protolude hiding (Enum)

import GraphQL.API
import GraphQL.Server
import qualified GraphQL.Internal.AST as AST
import GraphQL.Value (Value)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import GraphQL.Internal.Parser (document)

import qualified System.Directory as SD

type File = Object "File" '[]
  '[ Field "name" Text
   , Field "absolutePath" Text
   ]

type Directory = Object "Directory" '[]
  '[ Argument "glob" (Maybe Text) :> Field "entries" (List File)
   , Field "numEntries" Int32
   , Field "absolutePath" Text
   ]

type Query = Object "Query" '[]
  '[ Argument "path" (Maybe Text) :> Field "root" Directory ]


oneFile :: FilePath -> Handler IO File
oneFile path =
  pure $ pure ((toS @_ @Text) path)
    :<> map (toS @_ @Text) (SD.canonicalizePath path)

directory :: Maybe Text -> Handler IO Directory
directory Nothing = directory (Just "/")
directory (Just path) = do
  paths <- SD.listDirectory (toS path)
  pure $ filtered paths
    :<> pure (fromIntegral (length paths))
    :<> map (toS @_ @Text) (SD.canonicalizePath (toS path))
    where
      filtered :: [FilePath] -> Maybe Text -> [Handler IO File]
      filtered paths (Just glob) =
        map oneFile (filter (== (toS glob)) paths)
      filtered paths Nothing =
        map oneFile paths

root :: Handler IO Query
root = do
  pure directory

example :: IO (Result Value)
example = buildResolver @IO @Query root (query "{ root(path: \"/etc\") { entries { name } } }")

query :: Text -> AST.SelectionSet
query q =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
       parseOnly (document <* endOfInput) q
  in selectionSet
