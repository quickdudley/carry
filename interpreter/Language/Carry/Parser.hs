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
import Control.Lens.Setter

import Codec.Phaser.Core
import Codec.Phaser.Common
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
indentWith :: Monoid p => Phase p Char o a -> Phase p Char o a
indentWith = undefined
