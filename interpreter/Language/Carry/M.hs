{-# LANGUAGE TemplateHaskell #-}
{-|
Module:     Language.Carry.M
Copyright:  Jeremy List
License:    BSD-3
Maintainer: quick.dudley@gmail.com

Types representing the initially parsed AST and a monad for transforming it
in preparation for the next stage
-}
module Language.Carry.M (
  Name(..),
  SourceRegion(..),
  sourceModuleName,
  sourceUnitBegins,
  sourceUnitEnds,
  HasRegion(..),
  Type(..),
  Kind(..),
  ForallFloatability(..),
  TyConstraint(..),
  Pattern(..),
  Literal(..),
  Expression(..),
  DoStatement(..),
  ADirection(..),
  Declaration(..)
 ) where

import Data.Text
import qualified Data.Map as M
import Control.Lens
import Codec.Phaser.Common (Position(..))

import Language.Carry.Name
import Language.Carry.ForallFloat

data SourceRegion = SourceRegion {
  _sourceModuleName :: Text,
  _sourceUnitBegins :: Position,
  _sourceUnitEnds :: Position
 }

makeLenses ''SourceRegion

class HasRegion a where
  sourceRegion :: Lens' a SourceRegion

data Type =
  TyLambda SourceRegion [Name] [TyConstraint] Type |
  TyCon SourceRegion Name [Type]

instance HasRegion Type where
  sourceRegion f (TyLambda r n c i) = (\r' -> TyLambda r' n c i) <$> f r
  sourceRegion f (TyCon r n a) = (\r' -> TyCon r' n a) <$> f r

data Kind =
  KleeneStar SourceRegion | KindArrow SourceRegion ForallFloatability Kind Kind

instance HasRegion Kind where
  sourceRegion f (KleeneStar r) = KleeneStar <$> f r
  sourceRegion f (KindArrow r l a b) = (\r' -> KindArrow r' l a b) <$> f r

data TyConstraint = TyConstraint SourceRegion Name [Type]

instance HasRegion TyConstraint where
  sourceRegion f (TyConstraint r n a) = (\r' -> TyConstraint r' n a) <$> f r

data Pattern =
  ConstructorPattern SourceRegion Name |
  AppliedPattern SourceRegion Pattern Pattern |
  VariablePattern SourceRegion Name |
  WildCardPattern SourceRegion

instance HasRegion Pattern where
  sourceRegion f (ConstructorPattern r n) =
    (\r' -> ConstructorPattern r' n) <$> f r
  sourceRegion f (AppliedPattern r a b) = (\r' -> AppliedPattern r' a b) <$> f r
  sourceRegion f (VariablePattern r n) = (\r' -> VariablePattern r' n) <$> f r
  sourceRegion f (WildCardPattern r) = WildCardPattern <$> f r

data Literal =
  StringLiteral String |
  IntegerLiteral Integer |
  FractionalLiteral Rational

data Expression =
  LiteralExpression SourceRegion Literal |
  ConstructorExpression SourceRegion Name |
  AppliedExpression SourceRegion Expression Expression |
  LambdaExpression SourceRegion Pattern Expression |
  CaseExpression SourceRegion
    Expression [([Pattern],Maybe Expression, Expression)] |
  LetExpression SourceRegion [Declaration] Expression |
  IfExpression SourceRegion Expression Expression Expression |
  DoExpression SourceRegion [DoStatement]

instance HasRegion Expression where
  sourceRegion f (LiteralExpression r l) =
    (\r' -> LiteralExpression r' l) <$> f r
  sourceRegion f (ConstructorExpression r n) =
    (\r' -> ConstructorExpression r' n) <$> f r
  sourceRegion f (AppliedExpression r a b) =
    (\r' -> AppliedExpression r' a b) <$> f r
  sourceRegion f (LambdaExpression r p s) =
    (\r' -> LambdaExpression r' p s) <$> f r
  sourceRegion f (CaseExpression r d c) =
    (\r' -> CaseExpression r' d c) <$> f r
  sourceRegion f (LetExpression r d s) =
    (\r' -> LetExpression r' d s) <$> f r
  sourceRegion f (IfExpression r d t n) =
    (\r' -> IfExpression r' d t n) <$> f r
  sourceRegion f (DoExpression r s) =
    (\r' -> DoExpression r' s) <$> f r

data DoStatement =
  DoBind SourceRegion Pattern Expression |
  DoAction SourceRegion Expression |
  DoLet SourceRegion [Declaration]

data ADirection = FixL | FixR deriving (Eq,Ord)

data Declaration =
  NormalDeclaration SourceRegion Pattern (Maybe Expression) Expression |
  FixityDeclaration SourceRegion [Name] ADirection |
  ClassDeclaration SourceRegion [TyConstraint] Name [Name] [Declaration]
