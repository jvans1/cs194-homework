map' :: (a -> b) -> [a] -> [b]
map' f = foldr fun [] 
  where 
    fun :: [a] -> [a] -> [a]
    fun = undefined
