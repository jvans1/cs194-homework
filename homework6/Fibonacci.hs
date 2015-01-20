fib :: Integer -> Integer
fib 0 = 0 
fib 1 = 0
fib 2 = 1
fib n = fib(n - 1) + fib(n - 2)


fibs1 :: [Integer]
fibs1 = foldr (\x y -> (fib x):y) [] [0..]


fibs2 :: [Integer]
fibs2 = 0 : 1 : [x + y | (x, y) <- zip fibs2 (drop 1 fibs2)]


data Stream = jjkg
streamToList :: Stream a -> [a]
streamToList kk
