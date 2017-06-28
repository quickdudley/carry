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
  IfExpression SourceRegion Expression Expression Expression

data Declaration =
  NormalDeclaration SourceRegion Pattern
