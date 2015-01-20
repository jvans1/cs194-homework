import Data.List
sieveSundaram :: Integer -> [Integer]
sieveSundaram = nub . makePrime . generateList
  where
    makePrime :: [Integer] -> [Integer]
    makePrime l = map (\x -> 2 * x + 1 ) l
    generateList :: Integer -> [Integer]
    generateList n = map fst $ filter meetsCond $ cartProd [1..n] [1..n]
      where 
        meetsCond :: (Integer, Integer) -> Bool
        meetsCond (i, j) =  not $ i + j + (2 * i * j) <= n

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
