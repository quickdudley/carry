module Language.Carry.ForallFloat (
  ForallFloatability(..)
 ) where

import Data.List (and,zipWith)

data ForallFloatability =
  Impermeable | InOnly | OutOnly | Permeable deriving (Eq,Ord)

instance Monoid ForallFloatability where
  mempty = Permeable
  mappend Impermeable _ = Impermeable
  mappend Permeable b = b
  mappend InOnly OutOnly = Impermeable
  mappend InOnly InOnly = InOnly
  mappend OutOnly OutOnly = OutOnly
  mappend a b = mappend b a

infixl 7 =<=
(=<=) :: ForallFloatability -> ForallFloatability -> Bool
a =<= b = let
  d Impermeable = [False, False]
  d InOnly = [True, False]
  d OutOnly = [False,True]
  d Permeable = [True,True]
  in and $ zipWith ((||) . not) (d a) (d b)
