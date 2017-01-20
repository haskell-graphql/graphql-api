-- | Basic tokenising used by parser.
module GraphQL.Internal.Syntax.Tokens
  ( tok
  , whiteSpace
  ) where

import Protolude
import Data.Attoparsec.Text
  ( Parser
  , anyChar
  , endOfLine
  , peekChar
  , manyTill
  )
import Data.Char (isSpace)

tok :: Parser a -> Parser a
tok p = p <* whiteSpace

whiteSpace :: Parser ()
whiteSpace = peekChar >>= traverse_ (\c ->
  if isSpace c || c == ','
    then anyChar *> whiteSpace
    else when (c == '#') $ manyTill anyChar endOfLine *> whiteSpace)
