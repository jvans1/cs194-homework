{- XOverloadedStrings -}

module AParser where

import           Control.Applicative
import           Data.Text(strip)

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
{- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } -}

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser (\s -> fmap (first f) (runParser p s ))



-- (String -> Maybe ((a -> b), String)) -> (String -> Maybe (a, String) ) -> (String -> Maybe (b, String))
instance Applicative Parser where
  pure x = Parser (\s -> Just(x, s))
  (Parser extractFunc) <*> p =  Parser fn
    where
      fn s = case extractFunc s of
                  Nothing -> Nothing
                  Just (f, s') -> fmap (\(x, y) -> (f x, y))  $ runParser p s'


{- (String -> String -> (Char, Char)) Parser String Parser String -}

abParser :: Parser (Char, Char)
abParser = (\a b -> (a,b)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> char 'a' <*> char 'b'


intPair :: Parser [Integer]
intPair = (\x _ z -> x:z:[]) <$> posInt <*> char ' ' <*>  posInt


instance Alternative Parser where
  empty       = Parser (\s -> Nothing)
  (<|>) p1 p2 = Parser altFunc where
    altFunc s = (runParser p1 s) <|> (runParser p2 s)


intOrUppercase :: Parser ()
intOrUppercase = (\a -> ()) <$> posInt <|> (\a -> ()) <$> satisfy isUpper
