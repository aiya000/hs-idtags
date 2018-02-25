-- |
-- Expose newtypes of 'Text' to make program tokens of Idris.
--
-- This is based https://www.haskell.org/onlinereport/lexemes.html .
module Test.QuickCheck.IdTags.Token
  ( PascalName (..)
  , VarName (..)
  , OperatorName (..)
  ) where

import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..), elements, sublistOf)
import qualified Data.Text as T

-- |
-- Like "Konoko", "Sugar", and "FOOOGARRR"
-- (ascLarge ':' any (ascLarge | ascSmall))
newtype PascalName = PascalName
  { unDataName :: Text
  } deriving (Show)

instance Arbitrary PascalName where
  arbitrary = do
    x <- (:) <$> elements ['A'..'Z'] <*> sublistOf (['A'..'Z'] ++ ['a'..'z'])
    return . PascalName $ T.pack x


-- | Like "a", "abc" (any ascSmall)
newtype VarName = VarName
  { unVarName :: Text
  } deriving (Show)

instance Arbitrary VarName where
  arbitrary = VarName . T.pack <$> sublistOf ['a'..'z']


-- |
-- Like "<>", "<|>", and "<<<??>!!>!\@-~|" (any ascSymbol)
--
-- NOTICE: This doesn't contain the brackets (be not like "(<>)", "(<|>)", or else)
newtype OperatorName = OperatorName
  { unOperatorName :: Text
  } deriving (Show)

instance Arbitrary OperatorName where
  arbitrary = OperatorName . T.pack <$> sublistOf ascSym
    where
      ascSym :: [Char]
      ascSym = [ '!' , '#' , '$' , '%' , '&'
               , '*' , '+' , '.' , '/' , '<'
               , '=' , '>' , '?' , '@' , '\\'
               , '^' , '|' , '-' , '~'
               ]
