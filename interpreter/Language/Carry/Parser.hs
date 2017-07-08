{-# LANGUAGE ApplicativeDo,RankNTypes,OverloadedStrings #-}
-- Phaser runs faster with ApplicativeDo (or manually using "<*>" and friends)
module Language.Carry.Parser (
  moduleName
 ) where

import Data.Char
import Data.Void
import qualified Data.Text as T

import Control.Applicative
import Control.Monad

import Codec.Phaser.Core
import Codec.Phaser.Common
import Codec.Phaser.Indent
import Language.Carry.M

moduleName :: Monoid p => Phase p Char o [T.Text]
moduleName = sepBy
  (do
    a <- satisfy isAlpha
    r <- munch $ do
      c1 <- isAlphaNum
      c2 <- (== '_')
      return (c1 || c2)
    return (T.pack $ a : r)
   )
  (char '.')

stripComments :: Monoid p => Phase p Char Char ()
-- I know the phaser library has a type called Automaton: but actually writing
-- something this close to a finite state automaton with it is pretty uncommon.

-- This function also illustrates some of the power of the phaser library:
-- you can use 'trackPosition >># stripComments' and each component behaves
-- correctly.
stripComments = go where
  -- The disadvantage of ApplicativeDo is sometimes wanting to use >>= manually.
  go = (<|> return ()) $ get >>= \c -> case c of
    '{' -> goC1
    '-' -> goL1
    _ -> yield c >> go
  goC1 = (<|> (yield '{' >> eof)) $ get >>= \c -> case c of
    '-' -> goCR
    '{' -> yield '{' >> goC1
    _ -> yield '{' >> yield c >> go
  goL1 = (<|> (yield '-' >> eof)) $ get >>= \c -> case c of
    '-' -> goLR
    _ -> yield '-' >> yield c >> go
  goCR = get >>= \c -> case c of
    '-' -> goCN
    _ -> goCR
  goLR = get >>= \c -> case c of
    '\n' -> yield '\n' >> go
    _ -> goLR
  goCN = get >>= \c -> case c of
    '-' -> goCN
    '}' -> go
    _ -> goCR

-- Parse first, resolve later.
name :: Monoid p => Phase p Char o Name
name = do
  a <- satisfy isAlpha
  r <- munch $ do
    c1 <- isAlphaNum
    c2 <- (== '_')
    return (c1 || c2)
  return (UnresolvedName $ T.pack $ a : r)

isILS = (&&) <$> isSpace <*> (/='\n')

-- The unusual block indentation rule:
--
-- Since no-one agrees on how many spaces make up a tab: I'm not delimiting
-- blocks by counting the amount of white space. Instead: the whitespace before
-- each line must be the same sequence of whitespace characters as for other
-- lines in the same block. I feel it's the only way to be consistent, even
-- though some editors will require configuration.
newBlock :: IndentPhase a -> IndentPhase a
newBlock = blockWith $ do
  ci <- currentIndent
  let
    loop :: IndentPhase (Phase Position Char Void ())
    loop = do
      r <- liftPhase $ ci *> munch1 isILS
      e <- liftPhase get
      case e of
        '\n' -> loop
        _ -> liftPhase (put1 e) *> pure (void $ ci *> string r) 
  s <- liftPhase $ munch isILS
  e <- liftPhase get
  case e of
    '\n' -> loop
    _ -> if null s
      then fail "Missing space"
      else liftPhase (put1 e) *> pure ci

infixl 4 <*|>
(<*|>) :: IndentPhase (a -> b) -> IndentPhase a -> IndentPhase b
a <*|> b = a <*> newBlock b

infixl 1 >>|=
(>>|=) :: IndentPhase a -> (a -> IndentPhase b) -> IndentPhase b
a >>|= f = a >>= (newBlock <$> f)

infixl 1 >>|
(>>|) :: IndentPhase a -> IndentPhase b -> IndentPhase b
a >>| b = a *> newBlock b

infixl 4 <*|
(<*|) :: IndentPhase a -> IndentPhase b -> IndentPhase a
a <*| b = a <* newBlock b

-- Reimplement sepBy because different type
isb :: IndentPhase a -> IndentPhase b -> IndentPhase [a]
isb a s = isb1 a s <|> pure []

isb1 :: IndentPhase a -> IndentPhase b -> IndentPhase [a]
isb1 a s = (:) <$> a <*> many (s *> a)

type_p :: IndentPhase Type
type_p = do
  b <- liftPhase getCount
  let
    ty = liftPhase (char '\\') >>| do
      p <- isb (liftPhase name) space
      return () <|> space -- 'space' will run twice at once :(
      s <- liftPhase get
      case s of
        '.' -> innerType p [] -- sacrifice <*> to lord slowpoke
        '|' -> fail "Unimplemented: type constraint parser"
        _ -> fail "List of type variables must end in '|' or '.'"
    innerType p c = do
      i <- type_p
      e <- liftPhase getCount
      return (TyLambda (SourceRegion "" b e) p c i)
  ty
