{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinListBuffer where
import Scrabble
import Data.Monoid
import Sized
import Buffer
import JoinList

instance Buffer (JoinList (Score, Size) String) where
  toString Empty             = ""
  toString (Single a s)      = s
  toString (Append _ j1 j2)  =  (toString j1) ++ (toString j2)

  fromString  "" = Empty
  fromString  s = Single (score, 0) s
    where 
      score = scoreString s

  line a = indexJ a

  replaceLine i _ _ | i < 0 = Empty
  replaceLine _ _ Empty = Empty
  replaceLine i _ b1 | i > (numLines b1) = b1
  replaceLine i s (Single _ v)      = Single ((scoreString s), 0) s
  replaceLine i s (Append _ j1 j2)  = newLeft +++ (Single ((scoreString s), Size 0) s) +++ newRight
    where 
      newLeft = takeJ (i - 1) j1
      newRight = dropJ (i + 1) j2

  numLines = getSize . size . tag

  value   j = getScore j
    where 
      getScore :: JoinList (Score, Size) String -> Int
      getScore Empty                     = 0
      getScore (Single ((Score i), _) _)   = i
      getScore (Append ((Score i), _) _ _) = i
