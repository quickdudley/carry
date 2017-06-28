{-# LANGUAGE OverloadedStrings #-}
module Language.Carry.Types (

 ) where

import Data.Text
import Data.List

import Language.Carry.Name
import Language.Carry.M

applyTypeArguments :: Type -> [Type] -> Type
applyTypeArguments t [] = t
applyTypeArguments (TyLambda i c t) a = TyLambda i c (applyTypeArguments t a)
applyTypeArguments (TyCon n a1) a2 = TyCon n (a1 ++ a2)
