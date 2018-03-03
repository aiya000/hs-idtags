{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Expose randomly generatable stuff for the property based test cases ('IdTags.ParserTest' or else)
module Test.QuickCheck.IdTags
  ( DataTypeCode (..)
  , unDataTypeCode
  ) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import System.Random (Random(..))
import Test.QuickCheck (Arbitrary(..), Gen, choose)
import Test.QuickCheck.IdTags.Token (PascalName(..), SignName(..), CamelName(..))
import qualified Data.Text as T

-- | Please see 'IdTags.ParserTest.test_parser_parses_data_types' and this 'Random' instance
data DataTypeCode =
  -- |
  -- A type (constructor) but it isn't an operator.
  -- A data that has zero or more type arguments, like "data Foo" and "data Foo a".
  --
  -- Be generated based on 'PascalName' (the type name) and 'CamelName' (the argument names)
  NonOperatorTypeConstr Text
  -- |
  -- A type (constructor) of an operator.
  -- A data as a type operator, like "data (<!>) a b".
  --
  -- Be generated based on 'SignName' (the type name) and 'CamelName' (the argument names)
  | OperatorTypeConstr Text
  deriving (Show)

-- | Unwrap a 'Text' from either 'NonOperatorTypeConstr' or 'OperatorTypeConstr'
unDataTypeCode :: DataTypeCode -> Text
unDataTypeCode (NonOperatorTypeConstr x) = x
unDataTypeCode (OperatorTypeConstr    x) = x

instance Bounded DataTypeCode where
  minBound = NonOperatorTypeConstr $ unPascalName minBound
  maxBound = OperatorTypeConstr $ unSignName maxBound

instance Random DataTypeCode where
  randomR (NonOperatorTypeConstr x, NonOperatorTypeConstr y) gen =
    let (PascalName z, nextGen) = randomR (PascalName x, PascalName y) gen
    in (NonOperatorTypeConstr z, nextGen)
  randomR (OperatorTypeConstr x, OperatorTypeConstr y) gen =
    let (SignName z, nextGen) = randomR (SignName x, SignName y) gen
    in (OperatorTypeConstr z, nextGen)
  randomR (NonOperatorTypeConstr x, _) gen = randomR (NonOperatorTypeConstr x, NonOperatorTypeConstr x) gen
  randomR (OperatorTypeConstr    x, _) gen = randomR (OperatorTypeConstr x, OperatorTypeConstr x) gen
  random = randomR (minBound, maxBound)

instance Arbitrary DataTypeCode where
  arbitrary = (,) <$> asTheNonOperator <*> asTheOperator >>= choose
    where
      asTheNonOperator :: Gen DataTypeCode
      asTheNonOperator = do
        PascalName x <- arbitrary
        xs <- map unCamelName <$> arbitrary
        return . NonOperatorTypeConstr $ "data " <> T.unwords (x:xs)

      asTheOperator :: Gen DataTypeCode
      asTheOperator = do
        SignName x <- ("(" <>) . (<> ")") <$> arbitrary
        let x' = T.filter isValidSymbolInIdris x
        xs <- map unCamelName <$> arbitrary
        return . NonOperatorTypeConstr $ "data " <> T.unwords (x':xs)

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
