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
  Type(..),
  TyConstraint(..)
 ) where

import Data.Text
import qualified Data.Map as M

import Codec.Phaser.Common (Position(..))

import Language.Carry.Name

data SourceRegion = SourceRegion Text Position Position

data Type =
  TyLambda SourceRegion [Name] [TyConstraint] Type |
  TyCon SourceRegion Name [Type]

data TyConstraint = TyConstraint SourceRegion Name [Type]

data Pattern =
  ConstructorPattern SourceRegion Name |
  AppliedPattern SourceRegion Pattern Pattern |
  VariablePattern SourceRegion Name |
  WildCardPattern SourceRegion

data Literal =
  StringLiteral String |
  IntegerLiteral Integer |
  FractionalLiteral Rational

data Expression =
  LiteralExpression SourceRegion Literal |
  ConstructorExpression SourceRegion Name |
  AppliedExpression SourceRegion Expression Expression |
  LambdaExpression SourceRegion Pattern Expression
  CaseExpression SourceRegion
    Expression [([Pattern],Maybe Expression, Expression)] |
  LetExpression SourceRegion [Declaration] Expression |
  IfExpression SourceRegion Expression Expression Expression |
  DoExpression SourceRegion [DoStatement]

data DoStatement =
  DoBind SourceRegion Pattern Expression |
  DoAction SourceRegion Expression |
  DoLet SourceRegion [Declaration]

data Declaration =
  NormalDeclaration SourceRegion Pattern (Maybe Expression) Expression |
  FixityDeclaration SourceRegion [Name] ADirection |
  ClassDeclaration SourceRegion [TyConstraint] Name [Name] [ClassMember]

data ClassMember =
  ClassFunction Name Type
