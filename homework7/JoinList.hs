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


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _     | i < 0           = Nothing
indexJ _ Empty                   = Nothing
indexJ i j     | (getSize (tag j)) < i = Nothing
