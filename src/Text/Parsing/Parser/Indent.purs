-- | This is purescript-port of Text.Parsing.Indent
-- | <https://hackage.haskell.org/package/indents-0.3.3/docs/Text-Parsec-Indent.html>, 05.07.2016.
-- | A module to construct indentation aware parsers. Many programming
-- | language have indentation based syntax rules e.g. python and Haskell.
-- | This module exports combinators to create such parsers.
-- |
-- | The input source can be thought of as a list of tokens. Abstractly
-- | each token occurs at a line and a column and has a width. The column
-- | number of a token measures is indentation. If t1 and t2 are two tokens
-- | then we say that indentation of t1 is more than t2 if the column
-- | number of occurrence of t1 is greater than that of t2.
-- |
-- | Currently this module supports two kind of indentation based syntactic
-- | structures which we now describe:
-- |
-- | - **Block**
-- |
-- |   A block of indentation /c/ is a sequence of tokens with
-- |   indentation at least /c/.  Examples for a block is a where clause of
-- |   Haskell with no explicit braces.
-- |
-- | - **Line fold**
-- |
-- |   A line fold starting at line /l/ and indentation /c/ is a
-- |   sequence of tokens that start at line /l/ and possibly continue to
-- |   subsequent lines as long as the indentation is greater than /c/. Such
-- |   a sequence of lines need to be /folded/ to a single line. An example
-- |   is MIME headers. Line folding based binding separation is used in
-- |   Haskell as well.
module Text.Parsing.Indent
  ( IndentParser
  , runIndent
  , withBlock
  , withBlock'
  , block
  , block1
  , indented
  , indented'
  , sameLine
  , sameOrIndented
  , checkIndent
  , withPos
  , indentAp
  , (<+/>)
  , indentNoAp
  , (<-/>)
  , indentMany
  , (<*/>)
  , indentOp
  , (<?/>)
  , Optional(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.State (State, StateT, evalState, evalStateT, gets)
import Control.Monad.State.Trans (get, put)
import Control.Monad.Trans.Class (lift)
import Data.List (List(..), many)
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (ParserT, ParseState(ParseState), fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe)
import Text.Parsing.Parser.Pos (Position(..), initialPos)
import Text.Parsing.Parser.String (string, oneOf)

-- | Indentation sensitive parser type. Usually @ m @ will
-- | be @ Identity @ as with any @ ParserT @
type IndentParser m s a = ParserT s (StateT Position m) a

-- | @ getPosition @ returns current position
-- | should probably be added to Text.Parsing.Parser.Pos
getPosition :: forall m s. (Monad m) => ParserT s m Position
getPosition = gets \(ParseState _ pos _) -> pos

-- | simple helper function to avoid typ-problems with MonadState instance
get' :: forall s m. Monad m => IndentParser m s Position
get' = do
  g <- lift get
  pure g

-- | simple helper function to avoid typ-problems with MonadState instance
put' :: forall m s. Monad m => Position -> IndentParser m s Unit
put' p = lift (put p)

sourceColumn :: Position -> Int
sourceColumn (Position {line: _, column: c}) = c

sourceLine :: Position -> Int
sourceLine (Position {line: l, column: _}) = l

setSourceLine :: Position -> Int -> Position
setSourceLine (Position {line: _, column: c}) l = Position {line: l, column: c}

biAp :: forall a b c. (a -> b) -> (b -> b -> c) -> a -> a -> c
biAp f c v1 v2 = c (f v1) (f v2)

-- | @ many1 @ should prabably be inside Text.Parsing.Parser.Combinators
many1 :: forall s m a. (Monad m) => ParserT s m a -> ParserT s m (List a)
many1 p = lift2 Cons p (many p)

symbol :: forall m. (Monad m) => String -> ParserT String m String
symbol name = (many $ oneOf [' ','\t']) *> (string name)

-- | `withBlock f a p` parses `a`
-- | followed by an indented block of `p`
-- | combining them with `f`.
withBlock :: forall m a b c s. Monad m => (a -> List b -> c) -> IndentParser m s a -> IndentParser m s b -> IndentParser m s c
withBlock f a p = withPos $ do
    r1 <- a
    r  <- optionMaybe $ indented *> block p
    case r of
      Nothing -> pure (f r1 Nil)
      Just r2 -> pure (f r1 r2)

-- | Like 'withBlock', but throws away initial parse result
withBlock' :: forall m a b s. Monad m => IndentParser m s a -> IndentParser m s b -> IndentParser m s (List b)
withBlock' = withBlock (flip const)

-- | Parses only when indented past the level of the reference
indented :: forall m s. Monad m => IndentParser m s Unit
indented = do
    pos <- getPosition
    s <- get'
    if biAp sourceColumn (<=) pos s then fail "not indented" else do
        put' $ setSourceLine s (sourceLine pos)
        pure unit

-- | Same as `indented`, but does not change internal state
indented' :: forall m s. Monad m => IndentParser m s Unit
indented' = do
    pos <- getPosition
    s <- get'
    if biAp sourceColumn (<=) pos s then fail "not indented" else pure unit

-- | Parses only when indented past the level of the reference or on the same line
sameOrIndented :: forall m s. Monad m => IndentParser m s Unit
sameOrIndented = sameLine <|> indented

-- | Parses only on the same line as the reference
sameLine :: forall m s. Monad m => IndentParser m s Unit
sameLine = do
    pos <- getPosition
    s   <- get'
    if biAp sourceLine (==) pos s then pure unit else fail "over one line"

-- | Parses a block of lines at the same indentation level
block1 :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s (List a)
block1 p = withPos $ do
    r <- many1 $ checkIndent *> p
    pure r

-- | Parses a block of lines at the same indentation level , empty Blocks allowed
block :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s (List a)
block p = withPos $ do
    r <- many $ checkIndent *> p
    pure r

-- | Parses using the current location for indentation reference
withPos :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s a
withPos x = do
    a <- get'
    p <- getPosition
    r <- put' p *> x
    put' a *> pure r

-- | Ensures the current indentation level matches that of the reference
checkIndent :: forall m s. Monad m => IndentParser m s Unit
checkIndent = do
    s <- get'
    p <- getPosition
    if biAp sourceColumn (==) p s then pure unit else fail "indentation doesn't match"

-- | Run the result of an indentation sensitive parse
runIndent :: forall a m. Monad m => (StateT Position m) a -> m a
runIndent = flip evalStateT initialPos

-- | `<+/>` is to indentation sensitive parsers what `ap` is to monads
indentAp :: forall m s a b. Monad m => IndentParser m s (a -> b) -> IndentParser m s a -> IndentParser m s b
indentAp a b = ap a $ sameOrIndented *> b

infixl 9 indentAp as <+/>

-- | Like `<+/>` but doesn't apply the function to the parsed value
indentNoAp :: forall m s a b. Monad m => IndentParser m s a -> IndentParser m s b -> IndentParser m s a
indentNoAp a b = lift2 const a $ sameOrIndented *> b

infixl 10 indentNoAp as <-/>

-- | Like `<+/>` but applies the second parser many times
indentMany :: forall m s a b. Monad m => IndentParser m s (List a -> b) -> IndentParser m s a -> IndentParser m s b
indentMany a b = ap a (many (sameOrIndented *> b))

infixl 11 indentMany as <*/>

-- | Data type used to optional parsing
data Optional m s a = Opt a (IndentParser m s a)

-- | Like `<+/>` but applies the second parser optionally using the `Optional` datatype
indentOp :: forall m s a b. Monad m => IndentParser m s (a -> b) -> Optional m s a -> IndentParser m s b
indentOp a (Opt b c) = ap a (option b (sameOrIndented *> c))

infixl 12 indentOp as <?/>

-- | Parses with surrounding brackets
indentBrackets :: forall a. IndentParser String a -> IndentParser String a
indentBrackets p = withPos $ pure identity <-/> symbol "[" <+/> p <-/> symbol "]"

-- | Parses with surrounding angle brackets
indentAngles :: forall a. IndentParser String a -> IndentParser String a
indentAngles p = withPos $ pure identity <-/> symbol "<" <+/> p <-/> symbol ">"

-- | Parses with surrounding braces
indentBraces :: forall a. IndentParser String a -> IndentParser String a
indentBraces p = withPos $ pure identity <-/> symbol "{" <+/> p <-/> symbol "}"

-- | Parses with surrounding parentheses
indentParens :: forall a. IndentParser String a -> IndentParser String a
indentParens p = withPos $ pure identity <-/> symbol "(" <+/> p <-/> symbol ")"
