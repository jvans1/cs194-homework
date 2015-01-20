import Data.Monoid
data JoinList m a = Empty
                        | Single m a
                        | Append m (JoinList m a) (JoinList m a)
                        deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty   j      = j
(+++) j       Empty  = j
(+++) j1 j2          = Append (mappend (tag j1) (tag j2)) j1 j2


tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
