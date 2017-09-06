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
import Data.Traversable
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
  allSourceRegions :: Traversal' a SourceRegion

data Type =
  TyLambda SourceRegion [Name] [TyConstraint] Type |
  TyCon SourceRegion Name [Type]

instance HasRegion Type where
  sourceRegion f (TyLambda r n c i) = (\r' -> TyLambda r' n c i) <$> f r
  sourceRegion f (TyCon r n a) = (\r' -> TyCon r' n a) <$> f r
  allSourceRegions f (TyLambda r n c i) = (\r' c' i' -> TyLambda r' n c' i') <$>
    f r <*>
    for c (allSourceRegions f) <*>
    allSourceRegions f i

data Kind =
  KleeneStar SourceRegion | KindArrow SourceRegion ForallFloatability Kind Kind

instance HasRegion Kind where
  sourceRegion f (KleeneStar r) = KleeneStar <$> f r
  sourceRegion f (KindArrow r l a b) = (\r' -> KindArrow r' l a b) <$> f r
  allSourceRegions f (KleeneStar r) = KleeneStar <$> f r
  allSourceRegions f (KindArrow r l a b) =
    (\r' a' b' -> KindArrow r' l a' b') <$>
    f r <*>
    allSourceRegions f a <*>
    allSourceRegions f b

data TyConstraint = TyConstraint SourceRegion Name [Type]

instance HasRegion TyConstraint where
  sourceRegion f (TyConstraint r n a) = (\r' -> TyConstraint r' n a) <$> f r
  allSourceRegions f (TyConstraint r n a) = (\r' a' -> TyConstraint r' n a') <$>
    f r <*>
    for a (allSourceRegions f)

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
  allSourceRegions f (ConstructorPattern r n) =
    (\r' -> ConstructorPattern r' n) <$> f r
  allSourceRegions f (AppliedPattern r a b) = AppliedPattern <$>
    f r <*>
    allSourceRegions f a <*>
    allSourceRegions f b
  allSourceRegions f p = sourceRegion f p

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
  allSourceRegions f (AppliedExpression r a b) = AppliedExpression <$>
    f r <*>
    allSourceRegions f a <*>
    allSourceRegions f b
  allSourceRegions f (LambdaExpression r p s) = LambdaExpression <$>
    f r <*>
    allSourceRegions f p <*>
    allSourceRegions f s
  allSourceRegions f e = sourceRegion f e

data DoStatement =
  DoBind SourceRegion Pattern Expression |
  DoAction SourceRegion Expression |
  DoLet SourceRegion [Declaration]

instance HasRegion DoStatement where
  sourceRegion f (DoBind r p e) = (\r' -> DoBind r' p e) <$> f r
  sourceRegion f (DoAction r e) = (\r' -> DoAction r' e) <$> f r
  sourceRegion f (DoLet r d) = (\r' -> DoLet r' d) <$> f r
  allSourceRegions f (DoBind r p e) = DoBind <$>
    f r <*>
    allSourceRegions f p <*>
    allSourceRegions f e
  allSourceRegions f (DoAction r e) = DoAction <$>
    f r <*>
    allSourceRegions f e
  allSourceRegions f (DoLet r d) = DoLet <$>
    f r <*>
    for d (allSourceRegions f)

data ADirection = FixL | FixR deriving (Eq,Ord)

data Declaration =
  NormalDeclaration SourceRegion Pattern (Maybe Expression) Expression |
  FixityDeclaration SourceRegion [Name] ADirection |
  ClassDeclaration SourceRegion [TyConstraint] Name [Name] [Declaration]

instance HasRegion Declaration where
  sourceRegion f (NormalDeclaration r p g e) =
    (\r' -> NormalDeclaration r' p g e) <$> f r
  sourceRegion f (FixityDeclaration r n d) =
    (\r' -> FixityDeclaration r' n d) <$> f r
  sourceRegion f (ClassDeclaration r c n p m) =
    (\r' -> ClassDeclaration r' c n p m) <$> f r
  allSourceRegions f (NormalDeclaration r p g e) = NormalDeclaration <$>
    f r <*>
    allSourceRegions f p <*>
    for g (allSourceRegions f) <*>
    allSourceRegions f e
  allSourceRegions f d@(FixityDeclaration _ _ _) = sourceRegion f d
  allSourceRegions f (ClassDeclaration r c n p m) =
    (\r' c' m' -> ClassDeclaration r' c' n p m') <$>
    f r <*>
    for c (allSourceRegions f) <*>
    for m (allSourceRegions f)
