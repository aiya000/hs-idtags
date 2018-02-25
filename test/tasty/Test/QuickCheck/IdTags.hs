{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Expose randomly generatable stuff for the property based test cases ('IdTags.ParserTest' or else)
module Test.QuickCheck.IdTags
  (
  ) where

import Data.Text (Text)
import System.Random (Random(..), RandomGen(..))
import Test.QuickCheck (Arbitrary(..), elements, sublistOf, choose)
import Test.QuickCheck.IdTags.Token (PascalName(..), OperatorName(..), VarName(..))
import qualified Data.Text as T

-- | A workaround for the 'Random' orphan instance
newtype List' a = List' [a]

instance (Bounded a, Random a) => Random (List' a) where
  randomR :: forall g. RandomGen g => (List' a, List' a) -> g -> (List' a, g)
  randomR (List' xs, List' ys) gen =
    let randomR' :: a -> a -> (a, g)
        randomR' = curry $ flip randomR gen
        zs' :: [(a, g)]
        zs' = zipWith randomR' xs ys
    in case zs' of
        [] -> (List' [], gen)
        ((z, nextGen):zs) -> (List' (z : map fst zs), nextGen)

  --random :: StdGen -> (List' a, StdGen)
  random gen = randomR (minBound, maxBound) gen

instance Bounded a => Bounded (List' a) where
  minBound = List' []
  maxBound = List' $ replicate limitedSize maxBound
    where
      -- In this module, I may want finite lists
      limitedSize = 10000


-- | A workaround for the 'Random' orphan instance
newtype Text' = Text' Text

instance Random Text' where
  randomR (Text' x, Text' y) gen =
    let x' = T.unpack x
        y' = T.unpack y
        (List' randStr, nextGen) = randomR (List' x', List' y') gen
    in (Text' $ T.pack randStr, nextGen)

  random gen =
    let List' minimum = minBound
        List' maximum = maxBound
        minText = Text' $ T.pack minimum
        maxText = Text' $ T.pack maximum
    in randomR (minText, maxText) gen
