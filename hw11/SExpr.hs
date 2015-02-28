{- CIS 194 HW 11 due Monday, 8 April -}

module SExpr where

import Data.Char
import AParser
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
{- abParser :: Parser (Char, Char) -}
{- abParser = (\a b -> (a,b)) <$> char 'a' <*> char 'b' -}

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (\x y -> x ++ y) <$> arrayParser <*> (zeroOrMore p) <|> pure []
  where
    arrayParser = (\x -> x:[]) <$> p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (\x y -> x ++ y) <$> arrayParser <*> (oneOrMore p) <|> arrayParser
  where
    arrayParser = (\x -> x:[]) <$> p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (\x y -> x ++ y) <$> alphaParser <*> alphaNumParser where
  alphaParser = (\x -> x:[]) <$> satisfy isAlpha
  alphaNumParser = zeroOrMore $ satisfy isAlphaNum

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
