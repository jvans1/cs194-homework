module JoinList where
import Scrabble
import Data.Monoid
import Sized
data JoinList m a = Empty
                        | Single m a
                        | Append m (JoinList m a) (JoinList m a)
                        deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty   j      = j
(+++) j       Empty  = j
(+++) j1 j2          = Append (mappend (tag j1) (tag j2)) j1 j2


tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


sizedIndex :: (Sized a, Monoid a) => JoinList a b -> Int
sizedIndex s = ( indexValue $ tag s) 


indexValue :: (Sized a, Monoid a) => a -> Int
indexValue = getSize . size 

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                      = Nothing
indexJ i (Single m a)               = Just a
indexJ i (Append m j1 j2) 
  | (sizedIndex j1) > i             = indexJ i j1
  | otherwise                       = indexJ (i - (sizedIndex j1) ) j2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a  
dropJ 0 j                 =  j
dropJ _ Empty             = Empty
dropJ _ (Single _ _)      = Empty
dropJ i (Append m j1 j2)  
  | i < (sizedIndex j1)   = (+++) j1 (dropJ i j2)
  | otherwise             = dropJ (i - (sizedIndex j1)) j2


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ j@(Single _ _) = j
takeJ i (Append m j1 j2)  
  | leftSize <= i = (+++) j1 $ takeJ (i - leftSize) j2
  | otherwise   = takeJ i j1
    where 
      leftSize = sizedIndex j1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
