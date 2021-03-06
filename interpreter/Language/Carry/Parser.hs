{-# LANGUAGE ApplicativeDo,RankNTypes,OverloadedStrings #-}
-- Phaser runs faster with ApplicativeDo (or manually using "<*>" and friends)
module Language.Carry.Parser (
  moduleName,
  sourceFile
 ) where

import Data.Bits
import Data.Char
import Data.List
import qualified Data.Text as T
import Data.Void
import Data.Word

import Control.Applicative
import Control.Monad
import Control.Lens.Setter
import Control.Lens.Getter

import Codec.Phaser.Core
import Codec.Phaser.Common
import Codec.Phaser.UTF8
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
  goLR = (<|> return ()) $ get >>= \c -> case c of
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
  m <- (do
    l <- moduleName
    char '.'
    case l of
     [] -> fail "\'.\' is not understood"
     _ -> return l
   ) <|> pure []
  a <- satisfy isAlpha
  r <- munch $ do
    c1 <- isAlphaNum
    c2 <- (== '_')
    return (c1 || c2)
  let n = T.pack $ a : r
  forM_ reservedWords $ \rw -> if rw == n
    then fail $ "Reserved word: " ++ show n
    else return ()
  return (UnresolvedName m n)

reservedWords :: [T.Text]
reservedWords = ["case","if","then","else","let","of","in"]

infixName :: Monoid p => Phase p Char o Name
infixName = (do
  m <- (do
    l <- moduleName
    char '.'
    case l of
      [] -> fail "\'.\' is not understood"
      _ -> return l
   ) <|> pure []
  s <- munch1 $ \c -> case generalCategory c of
    _ | c == '`' -> False
      | c == '_' -> False
      | c == '\'' -> False
      | c == '\"' -> False
    ConnectorPunctuation -> True
    DashPunctuation -> True
    OtherPunctuation -> True
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    _ -> False
  case s of
    "=" -> fail "Unexpected equals sign"
    "\\" -> fail "Unexpected backslash"
    "|" -> fail "Unexpected guard"
    _ -> return (UnresolvedName m $ T.pack s)
 ) <|> (char '`' *> name <* char '`')

isILS = (&&) <$> isSpace <*> (/='\n')
prefix :: Monoid p => String -> Phase p Char o String
prefix = go id where
  go acc [] = return (acc [])
  go acc (a:r) = (<|> (acc [] <$ eof)) $ get >>= \c -> if c == a
    then go (acc . (c:)) r
    else put1 c *> return (acc [])

-- The unusual block indentation rule:
--
-- Since no-one agrees on how many spaces make up a tab: I'm not delimiting
-- blocks by counting the amount of white space. Instead: the whitespace before
-- each line must be the same sequence of whitespace characters as for other
-- lines in the same block. I feel it's the only way to be consistent, even
-- though some editors will require configuration.
block :: Monoid p => Phase p Char o a -> Phase p Char o a
block = fmap snd . block'

block' :: Monoid p => Phase p Char o a -> Phase p Char o ([Char],a)
block' p = munch isILS >>
  (eof *> ((,) [] <$> p)) <|> (get >>= \nc -> case nc of
    '\n' -> line1
    _ -> put1 nc *> ((,) [] <$> p)
   )
 where
  line1 = munch1 isILS >>= \lead ->
    (eof *> ((,) lead <$> p)) <|> (get >>= \nc -> case nc of
      '\n' -> line1
      _ -> put1 nc *> (fmap ((,) lead) $ fromAutomaton $ consumeIndent True True lead >># p) <*
        verifyFinished lead
     )

jBlock :: Monoid p => Phase p Char o a -> Phase p Char o a
jBlock p = fromAutomaton (chainWith (,) line1 p) >>= \ ~(indent, r) -> case indent of
  Just lead -> verifyFinished lead >> return r
  Nothing -> verifyOneLine >> return r
 where
  line1 = (<|> pure Nothing) $ get >>= \c -> yield c >> case c of
    '\n' -> line2ws id <|> pure Nothing
    _ -> line1
  line2ws lead = get >>= \c -> if isSpace c
    then yield c >> line2ws (lead . (c:))
    else case lead [] of
      [] -> fail "No indented block"
      lead' -> yield c >> line2 lead'
  line2 lead = (<|> pure (Just lead)) $ get >>= \c -> yield c >> case c of
    '\n' -> line3 lead
    _ -> line2 lead
  line3 lead = foldr (\c r -> char c >> yield c >> r) (return ()) lead >> line2 lead

verifyFinished lead = buffer $ let
  vnl = (get >>= \c -> case c of
    '\n' -> vnp
    _ | isSpace c -> vnl
      | otherwise -> fail "Inner parser has finished but indented block has not"
   ) <|> return False
  vnp = let
    go [] = fail "Inner parser has finished but indented block has not"
    go (a:r) = (get >>= \c -> yield c *> case c of
      '\n' -> go lead
      _ | c == a -> go r
        | otherwise -> return True
     ) <|> return False
    in go lead
  in vnl

verifyOneLine :: Monoid p => Phase p Char o ()
verifyOneLine = buffer $ let
  line1 = (get >>= \c -> case c of
    '\n' -> yield c >> line2
    _ | isSpace c -> yield c >> line1
      | otherwise -> fail "Inner parser has finished but the line has not"
   ) <|> return False
  line2 = (get >>= \c -> case c of
    '\n' -> yield c >> line2
    _ | isSpace c -> yield c >> blankLine
      | otherwise -> yield c >> return True
   ) <|> return False
  blankLine = (get >>= \c -> case c of
    '\n' -> yield c >> line2
    _ | isSpace c -> yield c >> blankLine
      | otherwise -> fail "Inner parser has finished but indented block has not"
   ) <|> return False
  in line1

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

inLiteralChar :: Monoid p => Phase p Char o Char
inLiteralChar = get >>= \c -> case c of
  _ | isControl c -> fail "Unexpected control character"
  '\"' -> fail "Unescaped double quote"
  '\'' -> fail "Unescaped single quote"
  '\\' -> escaped
  _ -> return c
 where
  escaped = get >>= \c -> case c of
    'n' -> return '\n'
    '\"' -> return '\"'
    '\'' -> return '\''
    '\\' -> return '\\'
    '/' -> return '/'
    'b' -> return '\b'
    'f' -> return '\f'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> hesc
    _ -> fail "Unrecognised escape sequence"
  hesc = (\a b c d ->
     toEnum $ foldl1' (.|.) $
       zipWith shiftL (map digitToInt [a,b,c,d]) [12,8,4,0]
    ) <$>
    satisfy isHexDigit <*>
    satisfy isHexDigit <*>
    satisfy isHexDigit <*>
    satisfy isHexDigit

literalString :: Monoid p => Phase p Char o String
literalString = char '\"' *> go id where
  go acc = (inLiteralChar >>= \c -> go (acc . (c:))) <|>
    (qnl acc) <|>
    (("Expected closing double quote" <?> char '\"') *> pure (acc []))
  qnl acc = string "\\\n" *>
    munch isSpace *>
    char '\\' *>
    go acc

literalChar :: Monoid p => Phase p Char o Char
literalChar = char '\'' *> inLiteralChar <* ("Expected closing single quote" <?> char '\'')

literal :: Monoid p => Phase p Char o Literal
literal = (StringLiteral <$> literalString) <|>
  (CharLiteral <$> literalChar) <|>
  (IntegerLiteral <$> regular) <|>
  (FractionalLiteral <$> (fromAutomaton $ requirePoint >># regular))
 where
  requirePoint = get >>= \c -> yield c *> case c of
    '.' -> mayEnd
    _ -> requirePoint
  mayEnd = (<|> pure ()) $ get >>= \c -> yield c *> mayEnd

lambdaExpression :: Phase Position Char o Expression
lambdaExpression = withRegion $ do
  char '\\'
  munch isSpace
  p <- pattern `sepBy` munch1 isSpace
  munch isSpace
  string "->"
  r <- block $ do
    munch isSpace
    expression
  return $ \s -> LambdaExpression s p r

caseExpression :: Phase Position Char o Expression
caseExpression = withRegion $ do
  string "case"
  munch1 isSpace
  cond <- expression
  munch1 isSpace
  string "of"
  cases <- block $ let
    go1 p acc = do
      g <- (char '|' *> munch isSpace *> (Just <$> expression)) <|> pure Nothing
      munch isSpace
      string "->"
      r <- block expression
      let acc' = ((p,g,r):) . acc
      go1 p acc' <|> go acc' <|> pure (acc' [])
    go acc = do
      p <- pattern
      go1 p acc
    in go id
  return $ \s -> CaseExpression s cond cases

expression :: Phase Position Char o Expression
expression = (withRegion $ infixExpression >>= \l -> case l of
  [] -> fail "Expected an expression"
  [Right e] -> return (const e)
  -- TODO: Optimize this
  _ -> (return (\r -> InfixExpression r l)) <|> do
    rhs <- expressionR
    return (\r -> InfixExpression r (l ++ [Right rhs]))
 ) <|> expressionR

expressionR :: Phase Position Char o Expression
expressionR =
  lambdaExpression <|>
  caseExpression <|>
  ifExpression

-- Since we don't know operator precedence at parse time: we defer that step.
infixExpression :: Phase Position Char o [Either Name Expression]
infixExpression = sepBy (fmap Right expression' <|> fmap Left infixName) (munch1 isSpace)

expression' :: Phase Position Char o Expression
expression' = (withRegion $ (flip LiteralExpression) <$> literal) <|>
  listExpression <|>
  (char '(' *> expression <* char ')')

listExpression :: Phase Position Char o Expression
listExpression = withRegion $ flip ListExpression <$> listOf expression

ifExpression :: Phase Position Char o Expression
ifExpression = withRegion $ do
  string "if"
  munch1 isSpace
  c <- expression
  munch1 isSpace
  string "then"
  t <- block expression
  string "else"
  e <- block expression
  return $ \r -> IfExpression r c t e

pattern :: Phase Position Char o Pattern
pattern = withRegion $ (\l r -> case l of
  [Right p] -> p
  _ -> InfixPattern r l
 ) <$> infixPattern

infixPattern :: Phase Position Char o [Either Name Pattern]
infixPattern = sepBy (fmap Right pattern' <|> fmap Left infixName) (munch1 isSpace)

pattern' :: Phase Position Char o Pattern
pattern' = wildcardPattern <|>
  listPattern <|>
  fail "Pattern parser not fully implemented"

wildcardPattern :: Phase Position Char o Pattern
wildcardPattern = withRegion $ WildCardPattern <$ char '_'

listPattern :: Phase Position Char o Pattern
listPattern = withRegion $ flip ListPattern <$> listOf pattern

type_ :: Phase Position Char o Type
type_ = let
  base = monotype <|> polytype
  go t1 = return t1 <|> (do
    munch isSpace
    string "->"
    munch isSpace
    t2 <- base
    go $ TyCon (sourceUnitEnds .~ (t2 ^. (sourceRegion . sourceUnitEnds)) $ t1 ^. sourceRegion) (GlobalName ["Prelude"] "->") [t1,t2]
   )
  in base >>= go

monotype :: Phase Position Char o Type
monotype = withRegion $ do
  a <- name
  return (\r -> TyCon r a []) <|> do
    args <- sepBy typeArg (munch1 isSpace)
    return (\r -> TyCon r a args)

typeArg :: Phase Position Char o Type
typeArg = withRegion $
  ((\n r -> TyCon r n []) <$> name) <|>
  ((\t r -> (sourceRegion .~ r) t) <$> (char '(' *> munch isSpace *> type_ <* munch isSpace <* char ')'))

polytype :: Phase Position Char o Type
polytype = fail "polymorphic types not yet implemented"

declaration :: Phase Position Char o Declaration
declaration = fail "not implemented"

sourceFile :: Automaton Position Word8 Declaration ()
sourceFile = utf8_stream >>#
  trackPosition >>#
  stripComments >>#
  loop
 where
  loop :: Phase Position Char Declaration ()
  loop = (<|> eof) $ do
    many blankLine
    declaration >>= yield
    many blankLine
    loop
  blankLine = get >>= \c -> case c of
    '\n' -> return ()
    _ | isSpace c -> blankLine
      | otherwise -> fail "Not a blank line"
