module Language.Carry.ForallFloat (
  ForallFloatability(..),
  (=<=),
  reverseFloatability
 ) where

import Data.List (and,zipWith)

data ForallFloatability =
  Impermeable | InOnly | OutOnly | Permeable deriving (Eq,Ord,Show)

data FloatOrientation =
  NormalFloat | ReversedFloat | FloatLocked deriving (Eq,Ord,Show)

instance Monoid ForallFloatability where
  mempty = Permeable
  mappend Impermeable _ = Impermeable
  mappend Permeable b = b
  mappend InOnly OutOnly = Impermeable
  mappend InOnly InOnly = InOnly
  mappend OutOnly OutOnly = OutOnly
  mappend a b = mappend b a
  mconcat = foldr mappend mempty

instance Monoid FloatOrientation where
  mempty = NormalFloat
  mappend NormalFloat b = b
  mappend ReversedFloat ReversedFloat = NormalFloat
  mappend FloatLocked _ = FloatLocked
  mappend a b = mappend b a
  mconcat = foldr mappend mempty

infixl 7 =<=
(=<=) :: ForallFloatability -> ForallFloatability -> Bool
a =<= b = let
  d Impermeable = [False, False]
  d InOnly = [True, False]
  d OutOnly = [False,True]
  d Permeable = [True,True]
  in and $ zipWith ((||) . not) (d a) (d b)

reverseFloatability :: ForallFloatability -> ForallFloatability
reverseFloatability Impermeable = Impermeable
reverseFloatability InOnly = OutOnly
reverseFloatability OutOnly = InOnly
reverseFloatability Permeable = Permeable

applyFloatOrientation :: ForallFloatability -> FloatOrientation ->
  ForallFloatability
applyFloatOrientation a NormalFloat = a
applyFloatOrientation a ReversedFloat = reverseFloatability a
applyFloatOrientation _ FloatLocked = Impermeable

infixl 7 <+>
(<+>) :: FloatOrientation -> FloatOrientation -> FloatOrientation
FloatLocked <+> _ = FloatLocked
NormalFloat <+> ReversedFloat = FloatLocked
NormalFloat <+> NormalFloat = NormalFloat
ReversedFloat <+> ReversedFloat = ReversedFloat
a <+> b = b <+> a
