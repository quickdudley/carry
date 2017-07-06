{-# LANGUAGE OverloadedStrings #-}
module Language.Carry.Name (
  Name(..)
 ) where

import Data.Text

data Name = LocalName Text | UnresolvedName Text |
  GlobalName [Text] Text deriving (Eq,Ord,Show)
