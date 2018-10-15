{-# LANGUAGE ApplicativeDo,RankNTypes,OverloadedStrings #-}
-- Phaser runs faster with ApplicativeDo (or manually using "<*>" and friends)
module Language.Carry.Parser (
  moduleName
 ) where

import Data.Bits
import Data.Char
import Data.List
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
    '\"' -> yield c >> goS
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
  goS = get >>= \c -> case c of
    '\"' -> yield c >> go
    '\\' -> yield c >> goSB
    '\n' -> fail "Unterminated string literal"
    _ -> yield c >> goS
  goSB = get >>= \c -> case c of
    '\"' -> yield c >> goS
    '\\' -> yield c >> goS
    '\n' -> yield c >> goSL
    _ -> yield c >> goS
  goSL = get >>= \c -> case c of
    _ | isSpace c -> yield c >> goSL
    '\\' -> yield c >> goS
    _ -> fail "Unterminated string literal"

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
prefix :: Monoid p => String -> Phase p Char o String
prefix = go id where
  go acc [] = return (acc [])
  go acc (a:r) = get >>= \c -> if c == a
    then go (acc . (c:)) r <|> (put1 c *> return (acc []))
    else put1 c *> return (acc [])

-- The unusual block indentation rule:
--
-- Since no-one agrees on how many spaces make up a tab: I'm not delimiting
-- blocks by counting the amount of white space. Instead: the whitespace before
-- each line must be the same sequence of whitespace characters as for other
-- lines in the same block. I feel it's the only way to be consistent, even
-- though some editors will require configuration.
jBlock :: Monoid p => Phase p Char o a -> Phase p Char o a
jBlock = fromAutomaton . (start >>#) where
  start = (<|> return ()) $ get >>= \c -> yield c *> case c of
    '\n' -> line2
    _ -> start
  line2 = do
    lead <- munch isILS
    c <- get
    case c of
      '\n' -> line2
      _ -> put1 c *> consumeIndent True True lead

consumeIndent :: Monoid p => Bool -> Bool -> [Char] -> Phase p Char Char ()
consumeIndent rq full lead = let
  go = (<|> pure ()) $ get >>= \c' -> yield c' *> if c' == '\n'
    then linestart *> go
    else go
  linestart = let
    m _ [] = return ()
    m n (a:r) = get >>= \c' -> case () of
     _ | c' == a -> n `seq` m (n + 1) r
       | c' == '\n' -> m 0 lead
       | full -> fail "Unexpected end of indented block"
       | rq && n == 0 -> fail "Unexpected end of indented block"
       | otherwise -> put1 c'
    in m 0 lead
  in go

blockWithClose :: Monoid p => Bool -> (b -> c -> a) ->
  Phase p Char o b -> Phase p Char o c -> Phase p Char o a
blockWithClose rq f b c = start where
  start = do
    munch isILS
    get >>= \c' -> if c' == '\n'
      then step1
      else put1 c' *> (f <$> b <*> (munch isILS *> c))
  step1 = do
    lead <- munch isILS
    get >>= \c -> if c == '\n' then step1 else put1 c *> step2 lead
  step2 [] | rq = fail "Expected indented block"
  step2 lead = do
    b' <- fromAutomaton $ consumeIndent rq True lead >># b
    c' <- fromAutomaton $ consumeIndent False False lead >># c
    return $ f b' c'


listOf :: Monoid p => Phase p Char o a -> Phase p Char o [a]
listOf p = do
  char '['
  blockWithClose True const (sepBy p c) (munch isSpace *> char ']')
 where
  c = munch isSpace *> char ',' *> munch isSpace

-- Parse with this, then annotate the result with 'setFilename'
withRegion :: Phase Position i o (SourceRegion -> a) -> Phase Position i o a
withRegion p = do
  begins <- getCount
  f <- p
  ends <- getCount
  return (f $ SourceRegion "" begins ends)

literalString :: Monoid p => Phase p Char o String
literalString = char '\"' *> go id where
  go acc = satisfy (not . isControl) >>= \c -> case c of
    '\n' -> fail "Newline in string literal without \'\\\'"
    '\"' -> return (acc [])
    '\\' -> goQ acc
    _ -> go (acc . (c:))
  goQ acc = get >>= \c -> case c of
    '\n' -> munch isSpace *> char '\\' *> go acc
    'n' -> go (acc . ('\n':))
    '\"' -> go (acc . ('\"':))
    '\\' -> go (acc . ('\\':))
    '/' -> go (acc . ('/':))
    'b' -> go (acc . ('\b':))
    'f' -> go (acc . ('\f':))
    'r' -> go (acc . ('\r':))
    't' -> go (acc . ('\t':))
    'u' -> hesc acc
    _ -> fail "Unrecognised escape sequence"
  hesc acc = ((\a b c d ->
     toEnum $ foldl1' (.|.) $
       zipWith shiftL (map digitToInt [a,b,c,d]) [12,8,4,0]
    ) <$>
    satisfy isHexDigit <*>
    satisfy isHexDigit <*>
    satisfy isHexDigit <*>
    satisfy isHexDigit) >>= \c -> go (acc . (c:))

literal :: Monoid p => Phase p Char o Literal
literal = (StringLiteral <$> literalString) <|>
  (IntegerLiteral <$> regular) <|>
  (FractionalLiteral <$> (fromAutomaton $ requirePoint >># regular))
 where
  requirePoint = get >>= \c -> yield c *> case c of
    '.' -> mayEnd
    _ -> requirePoint
  mayEnd = (<|> pure ()) $ get >>= \c -> yield c *> mayEnd
