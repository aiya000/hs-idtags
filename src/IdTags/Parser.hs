-- | Parse syntaxes to the AST
module IdTags.Parser
  ( parse
  ) where

import Control.Exception.Safe (SomeException)
import Data.Text (Text)
import IdTags.AST (AST(..))
import Text.Parser.Combinators (Parsing(..))

-- | Parse any terms to 'AST' if the term is correct
parse :: Text -> Either SomeException AST
parse = undefined


parse' :: Parsing m => Text -> m AST
parse' = undefined
