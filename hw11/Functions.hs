import Control.Applicative
pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)
{- f = Maybe: the result is Nothing if either of the arguments is; if both are Just the result is Just their pairing. -}
{- f = []: pair computes the Cartesian product of two lists. -}
{- f = ZipList: pair is the same as the standard zip function. -}
{- f = IO: pair runs two IO actions in sequence, returning a pair of their results. -}
{- f = Parser: pair runs two parsers in sequence (the parsers consume consecutive sections of the input), returning their results as a pair. If either parser fails, the whole thing fails. -}



(*>) :: Applicative f => f a -> f b -> f b
(*>) _ fb = fb


sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA xs = foldr (\fx fxs -> (:) <$> fx <*> fxs) (pure []) xs

                          {- (a -> [b]) -> ([a] -> [[b]]) -}

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
{- mapA fs = map  -}

{- replicateA :: Applicative f => Int -> f a -> f [a] -}

