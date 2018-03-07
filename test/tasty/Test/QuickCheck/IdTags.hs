{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Expose randomly generatable stuff for the property based test cases ('IdTags.ParserTest' or else)
module Test.QuickCheck.IdTags
  ( DataTypeCode (..)
  , unDataTypeCode
  ) where

import Data.String.Here (i)
import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, shuffle, sublistOf, listOf)
import Test.QuickCheck.IdTags.Tokens (PascalName(..), SignName(..), CamelName(..))
import qualified Data.Text as T

-- | Please see 'IdTags.ParserTest.test_parser_parses_data_types' and this 'Random' instance
data DataTypeCode = DataCode Text         -- ^ Like "data Foo", "data Foo a", "data Foo = Bar", "data Foo = Bar | Baz", or "data Foo a b = Bar b a"
                  | OperatorDataCode Text -- ^ Like "data (<!>) a b", or "data (<!>) a b = Bar a b"
  deriving (Show)

-- | Unwrap a 'Text' from either 'DataCode' or 'OperatorDataCode'
unDataTypeCode :: DataTypeCode -> Text
unDataTypeCode (DataCode x) = x
unDataTypeCode (OperatorDataCode x) = x

instance Arbitrary DataTypeCode where
  arbitrary = oneof [genDataCode, genOperatorDataCode]
    where
      -- only the one of
      --              vvvvvvvvv  vvvvvv
      -- data Foo a b = Bar b a | Baz a
      genValueConstr :: [Text] -> Gen Text
      genValueConstr params = do
        PascalName x <- arbitrary
        params' <- shuffle params >>= sublistOf
        return [i|${x} ${T.unwords params'}|]

      -- only
      --              vvvvvvvvvvvvvvvvv
      -- data Foo a b = Bar b a | Baz a
      genValueConstrsClause :: [Text] -> Gen Text
      genValueConstrsClause params = do
        xs <- listOf $ genValueConstr params
        return $ case xs of
          [] -> ""
          _  -> [i|= ${T.intercalate " | " xs}|]

      -- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      -- data Foo a b = Bar b a | Baz a
      genDataCode :: Gen DataTypeCode
      genDataCode = do
        PascalName x <- arbitrary
        xs <- map unCamelName <$> arbitrary
        vs <- genValueConstrsClause xs
        return $ DataCode [i|data ${x} ${T.unwords xs} ${vs}|]

      -- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      -- data (<!>) a b = Bar b a | Baz a
      genOperatorDataCode :: Gen DataTypeCode
      genOperatorDataCode = do
        x  <- T.filter isValidSymbolInIdris . unSignName <$> arbitrary
        xs <- map unCamelName <$> arbitrary
        vs <- genValueConstrsClause xs
        return $ DataCode [i|data (${x}) ${T.unwords xs} ${vs}|]


-- |
-- Can be used as a operator token in Idris?
--
-- http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#data-types
isValidSymbolInIdris :: Char -> Bool
isValidSymbolInIdris = (`elem` symbols)
  where
    symbols = [ ':' , '+' , '-' , '*' , '\\'
              , '/' , '=' , '.' , '?' , '|'
              , '&' , '>' , '<' , '!' , '@'
              , '$' , '%' , '^' , '~' , '#'
              ]
