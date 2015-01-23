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
sizedIndex s = (getSize $ size $ tag s) - 1 

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _     | i < 0              = Nothing
indexJ _ Empty                      = Nothing
indexJ i j     | (sizedIndex j) < i = Nothing
indexJ i (Single m a)               = Just a
indexJ i (Append m j1 j2) 
  | (sizedIndex j1) > i             = indexJ i j1
  | otherwise                       = indexJ i j2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a  
dropJ i j 
  | (sizedIndex j) >= i    = Empty
  | i == 0                 = j 

{- dropJ i,/ -}



