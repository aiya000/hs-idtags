{-# LANGUAGE DuplicateRecordFields #-}

-- | Expose the abstract syntax tree
module IdTags.Types
  ( AST (..)
  , ExCtags (..)
  , TagField
  ) where

import Data.Text (Text)

-- |
-- IdTags AST, an abstract of tag files.
-- This is converted from codes,
-- and be converted to tag files.
data AST = AsExCtags ExCtags

-- | A format for exuberant-ctags
data ExCtags = ExCtags
  { tagName :: Text -- ^ An identifier
  , tagFile :: Text -- ^ A file path of where 'tagName' is in
  , tagAddress :: Either Int Text -- ^ A line number or a regex of where 'tagName' of 'tagFile' is existent
  , tagFields :: [TagField] -- ^ A exberant-ctags specific text (Please see 'TagField')
  } deriving (Show)

-- |
-- A optinal information of a key value pair.
-- this is often 'kind' (like 'f', 'm', 'c'), 'class', 'struct'.
--
-- Please see below for more specifications.
--
-- http://ctags.sourceforge.net/FORMAT
type TagField = (Text, Text)
