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
zeroOrMore p = combineArrays <$> arrayParser <*> zeroOrMore p <|> pure []
  where
    combineArrays x y =  x ++ y
    arrayParser = (\x -> x:[]) <$> p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = combineArrays <$> arrayParser <*> oneOrMore p <|> arrayParser
  where
    combineArrays x y =  x ++ y
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


-- Remove whitespace
-- Go through each word
  -- atom
    -- Determine int or ident
    -- Create SExpr from Atom
  -- sexpr
    -- Create sexpr from by repeating above

-- combine all sexpresssion created in each step
--

parseIdent :: Parser SExpr 
parseIdent  = (\x -> A $ I x) <$> ident

parseIntAtoms :: Parser SExpr 
parseIntAtoms = (\x -> A $ N x) <$> posInt

parseSpacesFromAtoms :: Parser SExpr
parseSpacesFromAtoms = spaces *> (parseIdent <|> parseIntAtoms)

{- parseNestedSExpr :: Parser SExpr -}
{- parseNestedSExpr = satisfy  -}

parseAtoms :: Parser SExpr
parseAtoms = flattenSExpr <$> zeroOrMore parseSpacesFromAtoms
 where
   flattenSExpr x 
    | (length x) == 1 = head x
    | otherwise       = Comb x

parseSExpr :: Parser SExpr
parseSExpr = parseAtoms <|> (\x -> Comb x) <$> (sexprContent *> parseSExpr)

sexprContent :: Parser String
sexprContent =  char '(' *> (zeroOrMore $ satisfy (/= ')'))

{- parseSExpr :: Parser SExpr -}
{- parseSExpr  = parseIdent <|> parseIntAtoms <|> sexprContents -}
