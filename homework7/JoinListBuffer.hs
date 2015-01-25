{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinListBuffer where
import Scrabble
import Data.Monoid
import Sized
import Buffer
import JoinList

toList :: JoinList a b -> [b]
toList Empty             = []
toList (Single a s)      = [s]
toList (Append _ j1 j2)  =  (toList j1) ++ (toList j2)

type Annotation = (Score, Size)
type EditorJoinList = JoinList (Score, Size) String

makeAnnotation :: String -> Annotation
makeAnnotation s = (scoreString s, Size 1)

push :: Monoid b => (a -> JoinList b a) -> JoinList b a -> a -> JoinList b a
push makeSingle list a = list +++ makeSingle a

makeSingle :: String -> EditorJoinList
makeSingle s = Single (makeAnnotation s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . toList

  fromString = (foldl (push makeSingle) Empty) . lines

  line = indexJ

  replaceLine i _ j | outOfBounds                 = j
    where
      outOfBounds = i < 0 || i > (numLines j)
  replaceLine _ _ Empty                           = Empty
  replaceLine i s (Single _ _)                    = Single ((scoreString s), (Size 0)) s
  replaceLine i s (Append _ j1 j2)                = newLeft +++ (Single ((scoreString s), (Size 0)) s) +++ newRight
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
