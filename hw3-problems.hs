-- CS 538, Spring 2019: Homework 3

-- **Your code should compile without warnings.**
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
 {-# OPTIONS -Wall -Werror #-}

-- Do not change the following lines!
{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# OPTIONS -fwarn-tabs #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- You might want some functions from these libraries:
import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad       (Monad, MonadPlus, mzero, mplus)
import Data.Char
import Data.List

-- Parsing is a common task for converting unstructured data (strings, bit
-- vectors, etc.) into structured data (programs, syntax trees, packets, etc.).
-- In this homework, we will develop a miniature Haskell parsing library from
-- scratch, along with a set of general parser combinators for building up more
-- complex parsers out of simpler parsers. Finally, we will use the parser
-- library to build a simple calculator: parse input expressions, and return the
-- numeric answer. The library will give you hands-on experience with some of
-- the Haskell features we have seen in class: typeclasses and monads.
--
-- We will provide the setup code for parsing, along with useful utilities. Your
-- task is to define the key logic and the combinators.

-- To start with, we define the type of Parsers. These are functions that take
-- an input state (PState) containing the input string, and try to extract a value of
-- type a from a string. If a value of type a is successfully extracted, it is
-- returned along with the resulting state in the parse result (PResult a).
newtype Parser a = MkParser { runParser :: PState -> (PState, PResult a) }

-- The parser state can contain a lot of auxiliary information. To keep it
-- simple, our parser state will include just two pieces of information: the
-- remaining string to be parsed, and an integer offset representing how many
-- characters we have parsed so far.
data PState = MkPState { toParse  :: String
                       , stOffset :: !Int
                       } deriving Show
-- ! is called a "strictness" annotation: it requires the offset to be evaluated
-- to an integer before it is wrapped up into a PState, rather than lazily. We
-- won't talk about these issues in this class, but thinking about performance
-- in Haskell requires deciding which parts should be strict, and which parts
-- should be lazy. 

-- Turning to the result, a parser's result is either a successful parsed value
-- of type a, or a parse error of some kind.
data PResult a = ParseOK a | ParseError PError

-- To provide the user with useful error messages, sophisticated parsers keep
-- track of a lot of side information. We will include three things: an integer
-- offset indicating where the parse error occurred, possibly information about
-- what item was found, and a list of items that were expected.
data PError = MkPError { erOffset :: !Int
                       , found    :: Maybe ErrorChunk
                       , expected :: [ErrorChunk]
                       } deriving (Eq, Show)

-- Each ErrorChunk consists of either just a string, or a special token
-- indicating the end of the string (for when we expect the string to end but it
-- does not, or when the string unexpectedly ends in the middle of parsing).
data ErrorChunk = Chunk String | EndOfInput deriving Eq

-- To print parser results and errors in a human-readable form, we define the
-- following Show instances.
instance Show a => Show (PResult a) where
  show (ParseOK x)    = show x
  show (ParseError e) = 
    let loc = "Error at position " ++ show (erOffset e) ++ "\n"
        got = maybe "" (\ec -> "Found: " ++ show ec ++ "\n") (found e)
        ex = if null (expected e)
             then ""
             else "Expected: " ++ show (expected e) ++ "\n"
    in loc ++ got ++ ex

instance Show ErrorChunk where
  show (Chunk str)  = str
  show EndOfInput = "EOF"

-- Part 1: Basic ways of combining parsers (20) 
--
-- For our parsing library, we want to combine simpler parsers to build more
-- complex parsers. The first step is to define how to combine parsers together.
-- Experience has shown certain that there are certain common patterns for
-- combining programs (not just parsers) together.
--
-- The first way to combine two parsers is to run one parser, then run the
-- second parser on the rest of the string. This combination pattern can be
-- summed up by defining operations to make Parser into a Monad.
instance Monad Parser where
  return = pReturn
  (>>=)  = pBind

-- Define the return operation, which gives a parser that always yields the
-- given value of type a without changing the parse state.
pReturn :: a -> Parser a
pReturn val = MkParser $ \ps -> (ps, ParseOK val)

-- Define the bind operation for Parsers. This should run the first parser, and
-- look at the result. If the result is an error, the second parser should be
-- ignored. Otherwise, use the first result to choose which parser to run next.
pBind :: Parser a
      -> (a -> Parser b)
      -> Parser b
pBind p f = MkParser $ \ps -> case runParser p ps of
    (ps2, ParseError perr) -> (ps2, ParseError perr)
    (ps2, ParseOK val) -> runParser (f val) ps2

-- The second way to combine two parsers is to try the first parser, or the
-- second parser. This pattern is modeled by the Alternative typeclass.
instance Alternative Parser where
  empty = pZero
  (<|>) = pPlus

-- The Alternative type class has an "empty" operation. This operation
-- should satisfy the following laws:
--
-- empty <|> p === p <|> empty === p
--
-- For parsers, empty is the parser that fails without changing the state.
pZero :: Parser a
pZero = MkParser $ \ps -> (ps, ParseError $ MkPError (stOffset ps) Nothing [])

-- Define the choice operator (<|>) on two parsers. The combined parser should
-- try the first parser, and if it fails, try the second parser. If the first
-- parser succeeds, the second parser should not be run. If both parsers fail,
-- then use the provided function mergeErrors to combine the two resulting
-- errors and states. (You will be using this operation a lot.)
pPlus :: Parser a
      -> Parser a
      -> Parser a
pPlus p1 p2= MkParser $ \ps -> case runParser p1 ps of
    (ps1, ParseOK val) -> (ps1, ParseOK val)
    (ps1, ParseError perr1) -> case runParser p2 ps of
        (ps2, ParseOK val) -> (ps2, ParseOK val)
        (ps2, ParseError perr2) -> let (ps3, perr3) = mergeErrors (ps1, perr1) (ps2, perr2)
            in (ps3, ParseError perr3)

mergeErrors :: (PState, PError)
            -> (PState, PError)
            -> (PState, PError)
mergeErrors (st1, e1) (st2, e2)
  | erOffset e1 > erOffset e2 = (st1, e1)
  | erOffset e1 < erOffset e2 = (st2, e2)
  | otherwise = (st1, MkPError off fnd ex)
  where
    off = erOffset e1
    fnd = case (found e1, found e2) of
            (Nothing, Nothing) -> Nothing
            (Just x , Nothing) -> Just x
            (Nothing, Just y ) -> Just y
            (Just x , Just _ ) -> Just x
    ex = nub $ expected e1 ++ expected e2

-- The MonadPlus typeclass describes things that are both Monad and Alternative.
instance MonadPlus Parser where
  mzero = pZero
  mplus = (<|>)

-- We now define some other useful operations on parsers. Keep an eye out later
-- for opportunities to use them---they can help you define parsers more
-- concisely.
instance Functor Parser where
  fmap = pMap

-- The fmap operation takes a function from a -> b, and transforms a Parser
-- producing a's into a Parser producing b's. The fmap operation can also be
-- written f <$> p.
pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = do { x <- p ; return $ f x }

-- The operations making Parser into an Applicative instance are a bit trickier
-- to read. The last two come in handy quite often. The first, p1 *> p2, runs
-- p1, forgets the parsed value, and then runs p2. The second, p1 <* p2, runs
-- p1, remembers the parsed value, and forgets the parsed value from running p2.
-- Both parsers are run, but you can think of the arrow as pointing to the
-- parser whose result is returned as the final parsed value.
instance Applicative Parser where
  pure  = pReturn
  (<*>) = pApp
  p1 *> p2 = do { p1 ; p2 }
  p1 <* p2 = do { x <- p1 ; p2 ; return x }

pApp :: Parser (a -> b) -> Parser a -> Parser b
pApp pf p = do f <- pf
               f <$> p

-- Now, we define some functions to run our parsers given an input string. These
-- functions will be useful for testing your parser in ghci. The parse function
-- runs a parser on an input string, returning the final result. The final
-- state is discarded.
parse :: Parser a 
      -> String
      -> PResult a
parse p input = snd . runParser p $ MkPState input 0

-- The parseTest operation does the same thing, except it returns a Maybe: this
-- holds the parsed value if the parse succeeded, or Nothing if was an error.
parseTest :: Parser a
          -> String
          -> Maybe a
parseTest p input =
  case parse p input of
    ParseOK val  -> Just val
    ParseError _ -> Nothing

-- Part 2: Basic parsers and combinators (35)
--
-- Now, we start building up basic parsers and combinators.
--
-- The base parser will parse a single character satisfying a predicate.
-- Concretely,
--
-- token predicate expected
--
-- should parse a single character c from the front of the string if (predicate
-- c) is true. Remember to update the state of the parser: the offset in PState
-- should be incremented by 1, and the remaining string to parse should be
-- updated.
--
-- If (predicate c) is not true, the parser should produce an ParseError at the
-- current offset indicating that it found letter c and it was expecting one of
-- the items in expected. If the string is empty, the parser should produce an
-- error indicating that it found the EndOfInput.
token :: (Char -> Bool)
      -> [ErrorChunk]
      -> Parser Char
token pr ec = MkParser $ \ps -> case toParse ps of
    (x:xs) -> if pr x then (MkPState xs (stOffset ps + 1), ParseOK x)
              else (ps, ParseError $ MkPError (stOffset ps) (Just $ Chunk [x]) ec)
    [] -> (ps, ParseError $ MkPError (stOffset ps) (Just EndOfInput) ec)

-- Use token to define single, which parses exactly the given character from the
-- front of the string. Just like token, it's fine if the input string contains
-- more characters.
single :: Char -> Parser Char
single c = token (c ==) [Chunk [c]]

-- GHCI TEST: parseTest (single 'a') "a" === Just 'a'
-- GHCI TEST: parseTest (single 'a') "ab" === Just 'a'
-- GHCI TEST: parseTest (single 'a') "c" === Nothing

-- GHCI TEST: parseTest ((single 'a' >> single 'b') <|> (single 'a' >> single 'c')) "ac" === Just 'c'
-- GHCI TEST: parseTest (single 'a' <|> (single 'a' >> single 'c')) "ac" === Just 'a'
-- GHCI TEST: parseTest (single 'a' >>= (\c -> single c)) "a" === Nothing
-- GHCI TEST: parseTest (single 'a' >>= (\c -> single c)) "aa" === Just 'a'
-- GHCI TEST: parseTest (single 'a' >>= (\c -> single 'b' >> single c)) "aba" === Just 'a'

-- eof succeeds exactly when the remaining string is empty, otherwise it fails.
eof :: Parser ()
eof = MkParser $ \ps -> case toParse ps of
    [] -> (ps, ParseOK ())
    str -> (ps, ParseError $ MkPError (stOffset ps) (Just $ Chunk str) [EndOfInput])

-- GHCI TEST: parseTest eof "" === Just ()
-- GHCI TEST: parseTest eof "nonempty" === Nothing

-- chunk is like token, except it parses a target string instead of a character.
chunk :: String
      -> Parser String
chunk cs = MkParser $ \st ->
  case stripPrefix cs (toParse st) of
    Nothing   -> (st, ParseError $ MkPError (stOffset st) Nothing [Chunk cs])
    Just rest -> (MkPState rest (stOffset st + length cs), ParseOK cs)

-- satisfy parses one character satisfying the predicate
satisfy :: (Char -> Bool)
        -> Parser Char
satisfy pr = token pr [Chunk "true"]

-- GHCI TEST: parseTest (satisfy (`elem` "aeiou")) "a" === Just 'a'
-- GHCI TEST: parseTest (satisfy (`elem` "aeiou")) "z" === Nothing

-- oneOf parses any one in a list of characters
oneOf :: String
      -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- Parser combinators build new parsers out of old parsers, typically combining
-- multiple parsers into a single big parser.
--
-- Many parser combinators can be defined for parsers producing any value, not
-- just characters and strings. Indeed, these combinators can often be defined
-- for anything with Monad or MonadPlus operations, not just Parser. To
-- demonstrate this generality, we have stated the combinators below with a
-- Monad/MonadPlus constraint instead of requiring the Parser type. Since Parser
-- is a Monad and a MonadPlus, you can think of "Parser" instead of "m"
-- everywhere below.
--
-- Above, we have already seen all the pieces you need to define these
-- combinators. You should not need to use MkParser, for instance.
-- You will probably find it easier to start using do-notation here.

-- between runs a parser sandwiched between two other parsers, think open and
-- close parentheses. Only the result of the middle parser is returned.
between :: Monad m
        => m open
        -> m close
        -> m a
        -> m a
between op cl p = do
  op
  res <- p
  cl
  return res

-- GHCI TEST: parseTest (between (single '<') (single '>') (chunk "abcde")) "<abcde>" === Just "abcde"
-- GHCI TEST: parseTest (between (single '<') (single '>') (chunk "abcde")) "<abcde" === Nothing
-- GHCI TEST: parseTest (between (chunk "op(") (chunk ")cl") (oneOf "xyz")) "op(x)cl" === Just 'x'

-- optional converts a parser that produces a's to a parser that produces Maybe
-- a's. If the parser succeeds it will wrap Just around the result, otherwise it
-- will return Nothing.
optional :: MonadPlus m
         => m a
         -> m (Maybe a)
optional p = do res <- p
                return $ Just res
         <|> return Nothing

-- GHCI TEST: parseTest (optional $ oneOf "aeiou") "a" === Just (Just 'a')
-- GHCI TEST: parseTest (optional $ oneOf "aeiou") "b" === Just Nothing

-- many repeats a parser while it succeeds, producing the list of parsed values.
--
-- (Hint: use optional to check if the parser succeeds. If it doesn't return [].
-- If it does, keep recursing.)
many :: MonadPlus m
     => m a
     -> m [a]
many p = do
  res <- optional p
  case res of
    Nothing -> return []
    Just r  -> do rest <- many p
                  return (r : rest)

-- GHCI TEST: parseTest (many $ oneOf "aeiou") "aeiou" === Just "aeiou"
-- GHCI TEST: parseTest (many $ oneOf "aeiou") "" === Just ""
-- GHCI TEST: parseTest (many $ oneOf "aeiou") "wxyz" === Just ""

-- some is the same as many, except that the parser must succeed at least once.
some :: MonadPlus m
     => m a
     -> m [a]
some p = do
    res <- p
    rest <- some p <|> return []
    return (res : rest)
       

-- GHCI TEST: parseTest (some $ oneOf "aeiou") "aeiou" === Just "aeiou"
-- GHCI TEST: parseTest (some $ oneOf "aeiou") "" === Nothing
-- GHCI TEST: parseTest (some $ oneOf "aeiou") "wxyz" === Nothing

-- endBy takes a main parser p and a separator parser s, and runs
--
-- p s p s ... p s
--
-- as long as the parsers succeed (possibly zero times), always ending with an
-- s. The parsed separators are ignored, and the list of parsed values produced
-- by p is returned.
endBy :: MonadPlus m
      => m a
      -> m sep
      -> m [a]
endBy p ps = many (p <* ps)

-- GHCI TEST: parseTest (endBy (oneOf "aeiou") (oneOf ",.")) "a,e.i," === Just "aei"
-- GHCI TEST: parseTest (endBy (oneOf "aeiou") (oneOf ",.")) "" === Just ""
-- GHCI TEST: parseTest (endBy (oneOf "aeiou") (oneOf ",.")) "a,e.i," === Just "aei"
-- GHCI TEST: parseTest (endBy (oneOf "aeiou") (oneOf ",.")) "a" === Just ""

-- endBy1 is the same as endBy except that the parsers p and s must succeed at
-- least once.
endBy1 :: MonadPlus m
       => m a
       -> m sep
       -> m [a]
endBy1 p ps = some (p <* ps)

-- GHCI TEST: parseTest (endBy1 (oneOf "aeiou") (oneOf ",.")) "a,e.i," === Just "aei"
-- GHCI TEST: parseTest (endBy1 (oneOf "aeiou") (oneOf ",.")) "" === Nothing
-- GHCI TEST: parseTest (endBy1 (oneOf "aeiou") (oneOf ",.")) "a" === Nothing

-- sepBy takes a main parser p and a separator parser s, and tries
--
-- p s p s ... s p
--
-- ending with p, returning the list of values parsed by p. sepBy should produce
-- an empty list of elements if p fails immediately.
sepBy :: MonadPlus m
      => m a
      -> m sep
      -> m [a]
sepBy p ps = sepBy1 p ps <|> return []

-- GHCI TEST: parseTest (sepBy (oneOf "aeiou") (oneOf ",.")) "a,e.i" === Just "aei"
-- GHCI TEST: parseTest (sepBy (oneOf "aeiou") (oneOf ",.")) "" === Just ""
-- GHCI TEST: parseTest (sepBy (oneOf "aeiou") (oneOf ",.")) "a,e." === Just "ae"
-- GHCI TEST: parseTest (sepBy (oneOf "aeiou") (oneOf ",.")) "a, e.i" === Just "a"

-- sepBy1 is similar to sepBy, except that p must succeed at least once.
sepBy1 :: MonadPlus m
       => m a
       -> m sep
       -> m [a]
sepBy1 p ps = do
--    res <- some (p <* ps)
--    do {lst <- p; return (res ++ [lst])} <|> return res  --equivalent to: (p >>= (\lst -> return (res ++ [lst]))) <|> return res
      res <- p
      rest <- many (ps *> p)
      return (res : rest)  --easier to read this way

-- GHCI TEST: parseTest (sepBy1 (oneOf "aeiou") (oneOf ",.")) "a,e.i" === Just "aei"
-- GHCI TEST: parseTest (sepBy1 (oneOf "aeiou") (oneOf ",.")) "a,e." === Just "ae"
-- GHCI TEST: parseTest (sepBy1 (oneOf "aeiou") (oneOf ",.")) "" === Nothing

-- skipMany parses p as long as it is true, throwing away the parsed results.
skipMany :: MonadPlus m
         => m a
         -> m ()
skipMany p = many p *> return ()

-- GHCI TEST: parseTest (skipMany (oneOf "aeiou")) "aei" === Just ()
-- GHCI TEST: parseTest (skipMany (oneOf "aeiou")) "" === Just ()
-- GHCI TEST: parseTest (skipMany (oneOf "aeiou")) "xyz" === Just ()

-- skipSome is the same as skipMany, except p must successfully parse at least
-- once. All results should be thrown away, returning just ().
skipSome :: MonadPlus m
         => m a
         -> m ()
skipSome p = some p *> return ()

-- GHCI TEST: parseTest (skipSome (oneOf "aeiou")) "aei" === Just ()
-- GHCI TEST: parseTest (skipSome (oneOf "aeiou")) "" === Nothing
-- GHCI TEST: parseTest (skipSome (oneOf "aeiou")) "xyz" === Nothing

-- Part 3: Parsing arithmetic expressions (35)
--
-- We will now use these parsers to build a simple calculator: we will parse an
-- input string into an arithmetic expression, and then evaluate the expression.
--
-- First, we need a few parsers for space characters. space parses a single
-- space character.
space :: Parser Char
space = oneOf [' ', '\t', '\r', '\n']

-- Define a parser optSpaces that parses zero or more spaces.
optSpaces :: Parser ()
optSpaces = skipMany space

-- Define a parser spaces that parses one or more spaces.
spaces :: Parser ()
spaces = skipSome space

-- Define a parser symbol that parses a given string, followed by zero or more
-- spaces. The target string should be returned, and the spaces should be
-- discarded. This combinator is useful when the target string is a symbol, and
-- no spaces are needed afterwards.
--
-- (Hint: look at the Applicative operation <*)
symbol :: String
       -> Parser String
symbol str = chunk str <* optSpaces

-- GHCI TEST: parseTest (symbol "bobcat") "bobcat  " === Just "bobcat"
-- GHCI TEST: parseTest (symbol "bobcat") "bobcat" === Just "bobcat"
-- GHCI TEST: parseTest (symbol "bobcat") "bob" === Nothing

-- Define a parser keyword that parses a given string, followed by one or more
-- spaces. The target string should be returned, the spaces should be discarded.
-- This combinator is useful when the target string is a keyword of some kind,
-- where there must be at least one space afterwards to separate it from the
-- next character.
keyword :: String
        -> Parser String
keyword str = chunk str <* spaces

-- GHCI TEST: parseTest (keyword "bobcat") "bobcat  " === Just "bobcat"
-- GHCI TEST: parseTest (keyword "bobcat") "bobcat" === Nothing
-- GHCI TEST: parseTest (keyword "bobcat") "bob" === Nothing

-- The boolean parser parses "true" or "false" into a boolean.
boolean :: Parser Bool
boolean = (symbol "true" >> return True)
      <|> (symbol "false" >> return False)

-- We can build parsers for single digits and positive and negative numbers.
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

digitsToInt :: [Int]
            -> Int
digitsToInt = foldl (\cur new -> 10 * cur + new) 0
 
-- Using these helper functions, define a parser to parse a nonempty string of
-- numbers (possibly starting with "-") into an integer.
--
-- (Hint: a number is some digits, or a "-" followed by some digits.)
number :: Parser Int
number = (single '-' >> digitsToInt <$> some digit >>= ( \x -> return (-x)))
      <|> (digitsToInt <$> some digit)


-- GHCI TEST: parseTest number "12345" === Just 12345
-- GHCI TEST: parseTest number "-42" === Just (-42)
-- GHCI TEST: parseTest number "bob" === Nothing

-- Before parsing arithmetic expressions, we'll warm up with parsing a simpler
-- language of list expressions. A list expression is either:
-- (1) a list literal, just a list of numbers:
--
-- "[" num "," num "," ... "," num "]"
--
-- (2) the concatenation of a list literal and a list expression:
--
-- listliteral "~" listexpr
--
-- We model list expressions with the following datatype.
data SimpleLExpr = SimpleLSingle [Int]
                 | SimpleLConcat [Int] SimpleLExpr 
                 deriving Show

-- We define two versions of each parser. The unprimed version eats trailing
-- spaces while the primed version does not.
parseSimpleLLit :: Parser [Int]
parseSimpleLLit = parseSimpleLLit' <* optSpaces

-- Define a parser parsing a list literals: numbers separated by commas
-- (possibly with spaces) between square brackets.
parseSimpleLLit' :: Parser [Int]
parseSimpleLLit' = between (single '[' <* optSpaces) (single ']') $ sepBy (number <* optSpaces) $ single ',' <* optSpaces

-- GHCI TEST: parseTest parseSimpleLLit' "[1, 2]" === Just [1, 2]
-- GHCI TEST: parseTest parseSimpleLLit' "[1,2]" === Just [1, 2]
-- GHCI TEST: parseTest parseSimpleLLit' "[]" === Just []
-- GHCI TEST: parseTest parseSimpleLLit' "[-42," === Nothing
-- GHCI TEST: parseTest parseSimpleLLit' "[-42,]" === Nothing
-- GHCI TEST: parseTest parseSimpleLLit' "bob" === Nothing

parseSimpleLExpr :: Parser SimpleLExpr
parseSimpleLExpr = parseSimpleLExpr' <* optSpaces

-- Define a parser parsing a list literals, comma-separated lists of numbers
-- between square brackets.
--
-- (Hint: you'll want to choose (<|>) between two parsers: a simple list
-- literal, or a simple list literal followed by "~" and a simple list
-- expression. When using the choice combinator <|>, keep in mind that the
-- second parser is only run if the first parser fails. Order matters---p1 <|>
-- p2 is not the same as p2 <|> p1!)
parseSimpleLExpr' :: Parser SimpleLExpr
parseSimpleLExpr' = do
         x <- parseSimpleLLit
         y <- single '~' *> optSpaces *> parseSimpleLExpr
         return $ SimpleLConcat x y
    <|>  SimpleLSingle <$> parseSimpleLLit'

-- GHCI TEST: parseTest parseSimpleLExpr "[1, 2]" === Just (SimpleLSingle [1, 2])
-- GHCI TEST: parseTest parseSimpleLExpr "[1, 2]~[3]" === Just (SimpleLConcat [1, 2] (SimpleLSingle [3]))
-- GHCI TEST: parseTest parseSimpleLExpr "[1 ,2]  ~ [ 3]" === Just (SimpleLConcat [1, 2] (SimpleLSingle [3]))

--builds a parser of list expressions ignoring leading and trailing spaces.
parseSimpleLExpr'' :: Parser SimpleLExpr
parseSimpleLExpr'' = do
    optSpaces
    le <- parseSimpleLExpr
    eof
    return le

-- To get these list expressions into a more convenient form, we can evaluate
-- them to lists of integers.
evalSimpleL :: SimpleLExpr
           -> [Int]
evalSimpleL (SimpleLSingle ints)       = ints
evalSimpleL (SimpleLConcat ints lExpr) = ints ++ evalSimpleL lExpr

-- Now, we build a parser for arithmetic expressions. We will give datatypes
-- modeling the expression grammars in our calculator language. The grammar is a
-- bit complicated since it ensures that order of operations are followed
-- correctly, i.e., 1 + 2 * 3 is encoded as 1 + (2 * 3) and not (1 + 2) * 3.
--
-- An arithmetic expression is either:
-- (1) a single term:
--
-- term
--
-- (2) a term plus another arithmetic expression:
--
-- term "+" aexp
--
-- (3) an if-then-else expression:
--
-- "if" bexp "then" aexp "else" aexp
data AExpr = TSingle Term
           | Plus Term AExpr
           | IfThenElse BExpr AExpr AExpr
           deriving Show

-- A term is either:
-- (1) a single factor:
--
-- factor
--
-- (2) or a factor times a term:
--
-- factor "*" term
data Term = FSingle Fact
          | Mult Fact Term
          deriving Show

-- A factor is either:
-- (1) a string of digits, maybe negated:
--
-- "12345"
-- "0"
-- "-42"
-- ...
--
-- (2) or a single arithmetic expression surrounded by parentheses:
--
-- "(" aexp ")"
data Fact = AConst Int
          | ASingle AExpr
          deriving Show

-- Boolean expressions are either:
-- (1) a constant boolean:
--
-- "true"
-- "false"
--
-- (2) equality between two arithmetic expressions:
--
-- aexp "==" aexp
--
-- (3) or a comparison between two arithmetic expressions:
--
-- aexp "<" aexp
--
-- aexp ">" aexp
data BExpr = BConst Bool
           | BEq AExpr AExpr
           | BLt AExpr AExpr
           | BGt AExpr AExpr
           deriving Show

-- In the comments above each type, we wrote out what the input string should
-- look like for each of the cases. Literal characters are enclosed in quotes
-- (similar to the BNF grammars we saw in class)---the input string must match
-- exactly those characters (without the quotes, but maybe with spaces) for the
-- parse to succeed with that case.
--
-- Your parser should follow a few rules for parsing spaces:
--
-- (1) Multiple spaces are treated the same as a single space
--
-- (2) There must be at least one space around either side of an English keyword
-- ("true", "false", "if", "then", "else").
--
-- (3) Spaces are optional around keywords ("<", ">", "==", "@", "~").
parseFact :: Parser Fact
parseFact = parseFact' <* optSpaces

-- To warm up, here's a parser for Fact. We follow the grammar for Fact.
parseFact' :: Parser Fact
parseFact' = AConst <$> number
         <|> ASingle <$> between (symbol "(") (symbol ")") parseAExpr

parseTerm :: Parser Term
parseTerm = parseTerm' <* optSpaces

-- Define a parser to parse terms. You should use the combinators as much as
-- possible. Your parsers will be defined mutually recursively---parseFact uses
-- parseAExpr, which will use parseTerm, which will use parseFact again. This
-- will make it more difficult to test your parsers separately, since you kind
-- of need parsers for the whole grammar before you can test. Look down below
-- for some test cases (marked GHCI TEST).
parseTerm' :: Parser Term
parseTerm' = do
     fact <- parseFact
     single '*' <* optSpaces
     Mult fact <$> parseTerm'
  <|> FSingle <$> parseFact'

parseAExpr :: Parser AExpr
parseAExpr = parseAExpr' <* optSpaces

-- Define a parser to parse arithmetic expressions.
parseAExpr' :: Parser AExpr
parseAExpr' = do
     term <- parseTerm
     single '+' <* optSpaces
     Plus term <$> parseAExpr'
  <|>         do
     keyword "if"
     be <- parseBExpr
     keyword "then"
     ae1 <- parseAExpr
     keyword "else"
     IfThenElse be ae1 <$> parseAExpr'
 <|>          do
      TSingle <$> parseTerm'

parseBExpr :: Parser BExpr
parseBExpr = parseBExpr' <* optSpaces

-- Define a parser to parse boolean expressions.
parseBExpr' :: Parser BExpr
parseBExpr' = BConst <$> boolean
 <|>          do
      ae1 <- parseAExpr
      symbol "=="
      BEq ae1 <$> parseAExpr
     <|>          do
      ae1 <- parseAExpr
      symbol "<"
      BLt ae1 <$> parseAExpr
 <|>          do
      ae1 <- parseAExpr
      symbol ">"
      BGt ae1 <$> parseAExpr

-- Putting it all together, we can build a parser that parses an arithmetic
-- expression ignoring leading and trailing spaces.
parseCalc :: Parser AExpr
parseCalc = do
    optSpaces
    ae <- parseAExpr
    eof
    return ae

-- GHCI TEST: parseTest parseCalc "  0   "
--        === Just (TSingle (FSingle (AConst 0)))
-- GHCI TEST: parseTest parseCalc "-11 + 2"
--        === Just (TSingle (FSingle (ASingle (Plus (FSingle (AConst (-11))) (TSingle (FSingle (AConst 2)))))))
-- GHCI TEST: parseTest parseCalc "1 + 2 * 3"
--        === Just (Plus (FSingle (AConst 1)) (TSingle (Mult (AConst 2) (FSingle (AConst 3)))))
-- GHCI TEST: parseTest parseCalc "1*2+3"
--        === Just (Plus (Mult (AConst 1) (FSingle (AConst 2))) (TSingle (FSingle (AConst 3))))
-- GHCI TEST: parseTest parseCalc "1*(2+3)"
--        === Just (TSingle (Mult (AConst 1) (FSingle (ASingle (Plus (FSingle (AConst 2)) (TSingle (FSingle (AConst 3))))))))
-- GHCI TEST: parseTest parseCalc "1 *(2+ 3)" 
--        === Just (TSingle (Mult (AConst 1) (FSingle (ASingle (Plus (FSingle (AConst 2)) (TSingle (FSingle (AConst 3))))))))
-- GHCI TEST: parseTest parseCalc "1 + 2 + 3"
--        === Just (Plus (FSingle (AConst 1)) (Plus (FSingle (AConst 2)) (TSingle (FSingle (AConst 3)))))
-- GHCI TEST: parseTest parseCalc "if true then 1 else 0"
--        === Just (IfThenElse (BConst True) (TSingle (FSingle (AConst 1))) (TSingle (FSingle (AConst 0))))
-- GHCI TEST: parseTest parseCalc "if 2==3 then 1 else 0"
--        === Just (IfThenElse (BEq (TSingle (FSingle (AConst 2))) (TSingle (FSingle (AConst 3)))) (TSingle (FSingle (AConst 1))) (TSingle (FSingle (AConst 0))))
-- GHCI TEST: parseTest parseCalc "if true then if false then 1 else 0 else 2"
--        === Just (IfThenElse (BConst True) (IfThenElse (BConst False) (TSingle (FSingle (AConst 1))) (TSingle (FSingle (AConst 0)))) (TSingle (FSingle (AConst 2))))
--
-- GHCI TEST: parse parseCalc "1 + 2)"
--        === Error at position 5
--            Found: )
--            Expected: [EOF]
-- GHCI TEST: parse parseCalc "if true then 1"
--        === Error at position 14
--            Expected: [else]
-- GHCI TEST: parse parseCalc "(1 + 2"
--        === Error at position 6
--            Expected: [)]
-- GHCI TEST: parse parseCalc "if 2 then 3 else 4"
--        === Error at position 5
--            Expected: [==,<,>]
-- GHCI TEST: parse parseCalc "if true the 3 else 4"
--        === Error at position 8
--            Expected: [then]

-- To actually evaluate expressions, we need to convert AExpr/Term/Fact to
-- integers, BExpr to booleans, and LExpr to lists of integers.
evalA :: AExpr
      -> Int
evalA (TSingle term)               = evalT term
evalA (Plus term aexp)             = evalT term + evalA aexp
evalA (IfThenElse bexp aexp aexp') = if evalB bexp
                                     then evalA aexp
                                     else evalA aexp'

-- Do the same for terms, factors, and boolean expressions.
evalT :: Term
      -> Int
evalT (FSingle f)       = evalF f
evalT (Mult f t)        = evalF f * evalT t

evalF :: Fact
      -> Int
evalF (AConst n)        = n
evalF (ASingle ae)      = evalA ae

evalB :: BExpr
      -> Bool
evalB (BConst b)        = b
evalB (BEq ae1 ae2)     = evalA ae1 == evalA ae2
evalB (BLt ae1 ae2)     = evalA ae1  < evalA ae2
evalB (BGt ae1 ae2)     = evalA ae1  > evalA ae2


-- Our parsers only track the first parse. More typical languages can be parsed
-- in multiple ways. Imagine that we wanted to switch our ParseResult type to
-- produce a list of possible parses rather than just a single parse:
--
-- data PResult a = ParseOK [a] | ParseError PError
--
-- While this might seem like a big change, it turns out that we would only need
-- to change a few things in the Monad/MonadPlus instances describing the basic
-- combination operations for Parsers. Since just about everything else is built
-- on top of these operations, everything else would continue to work.

-- Part 4: Hooking up to I/O (10)
-- 
-- Finally, you will work with the I/O monad to enable your parsers to read
-- strings from the console, parse and evaluate the expressions, and then output
-- the result. While the monad is different---I/O instead of Parser---the
-- principles are the same.
--
-- Take a look at the standard operations putStr, putStrLn, and getLine.  The
-- parseIO function hooks a parser up to the console input/output by turning it
-- into something of type IO (). This operation should repeatedly ask for an
-- input string, read the input string, and print the result of running your
-- parser. If the user enters the string "q", parseIO should finish.
--
-- (Hint: you can build recursive computations in IO ().)
parseIO :: (Show a)
        => Parser a
        -> IO ()
parseIO p = do
    putStrLn "Enter arithmetic expression (or 'q' to quit):"
    x <- getLine
    case (x) of
     "q" -> putStrLn "\nHave a fantastic day! ;)\n\nUse main' instead to also accept simpleLExpr as input\n"
     inp   -> do
        putStr "\n= "
        print $ parse p inp
        putStr "\n"
        parseIO p
--included in case this is tested as part of grade. Otherwise prefer to use parseIO'


-- This is capable of either parsing and outputting the result of either an arithmetic expression
-- or a list concatenation
parseIO' :: IO ()
parseIO' = do
    putStrLn "Enter arithmetic expression or simple list expression (or 'q' to quit):"
    x <- getLine
    case (x) of
     "q" -> putStrLn "\nHave a fantastic day! ;)\n"
     inp   -> do
        putStr "\n= "
        print $ parse parseAEorLE inp
        putStr "\n"
        parseIO'

-- The main function lets you test your calculator parser by entering input.
main :: IO ()
main = parseIO (evalA <$> parseCalc)

main' :: IO ()
main' = printCalcASCII >> parseIO'


---To use <|>, parsers must be the same type. This type can contain the result of evaluating an arithmetic expression or a list expression
data AEorLE   = AResult Int
              | LResult [Int]

instance Show AEorLE where
   show (AResult i) = show i
   show (LResult i) = show i

--Parses and evaluates either an arithmetic expression or a simple list expression. If I didn't have other classes I probably could have implemented
------stuff like 5 + [4,4] = [9,9] or [4,4] + [3,3] = [7,7]
parseAEorLE :: Parser AEorLE
parseAEorLE = (AResult <$> evalA <$> parseCalc) <|> (LResult <$> evalSimpleL <$> parseSimpleLExpr'')
-- GHCI TEST: parseTest parseAEorLE "1 + 2 * 3"
--        === Just 7
-- GHCI TEST: parseTest parseAEorLE "1*2+3"
--        === Just 5
-- GHCI TEST: parseTest parseAEorLE "[1, 2]" === Just [1, 2]
-- GHCI TEST: parseTest parseAEorLE "[1, 2]~[3]" === Just [1, 2, 3]
-- GHCI TEST: parseTest parseAEorLE "[1 ,2]  ~ [ 3]" === Just [1, 2, 3]

--yayyy pretty picture! artwork from http://ascii.co.uk/ by Jeremy J. Olson 
printCalcASCII :: IO ()
printCalcASCII = do
    putStrLn "|  _________________  |\n| |              /  | |\n| |       /\\    /   | |\n| |  /\\  /  \\  /    | |\n| | /  \\/    \\/     | |"
    putStrLn "| |/             JO | |\n| |_________________| |\n|  __ __ __ __ __ __  |\n| |__|__|__|__|__|__| |\n| |__|__|__|__|__|__| |"
    putStrLn "| |__|__|__|__|__|__| |\n| |__|__|__|__|__|__| |\n| |__|__|__|__|__|__| |\n| |__|__|__|__|__|__| |\n|  ___ ___ ___   ___  |"
    putStrLn "| | 7 | 8 | 9 | | + | |\n| |___|___|___| |___| |\n| | 4 | 5 | 6 | | - | |\n| |___|___|___| |___| |\n| | 1 | 2 | 3 | | x | |"
    putStrLn "| |___|___|___| |___| |\n| | . | 0 | = | | / | |\n| |___|___|___| |___| |\n|_____________________|\n"
    putStrLn "Welcome to the calculator program!!!\n"


---------Didn't end up using these after I realized I could just create a new data type that can be an Int or an [Int]
------------------------------------------------------------------------------------------------------------------
--Basically the same thing as parseTest but only returns a boolean value.
pIsAnError :: Parser a -> String -> Bool
pIsAnError p st = case parse p st of
   ParseOK _    -> False
   ParseError _ -> True

--Returns the tuple of (PState, PError), which can be used to merge errors from Parsers of different types
pGetError :: Parser a -> String -> (PState, PError)
pGetError p str = case runParser p (MkPState str 0) of
    (_, ParseOK _) -> (MkPState str 0, MkPError 0 Nothing [])
    (ps, ParseError perr) -> (ps, perr)