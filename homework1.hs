toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | n < 10 = [n]
  | otherwise = (n `mod` 10) : toDigitsRev remaining
  where
    remaining :: Integer
    remaining = floor $ (fromIntegral n) / 10

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse $ mapWithIndex doubleEvens $ reverse n

doubleEvens :: (Integer, Int) -> Integer
doubleEvens (x, y)
  | y `mod` 2 == 0 = x 
  | otherwise = x * 2

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex fn xs = map fn (withIndices xs)
  where 
    withIndices :: [a] -> [(a, Int)]
    withIndices xs = zip xs [0..(length xs)]

sumDigits :: [Integer] -> Integer
sumDigits n = foldl (+) 0 $ concat $ map toDigits n


splitDigits :: [Integer] -> [Integer]
splitDigits n = concat $ map toDigits n

toDigitsDoubled :: Integer -> [Integer]
toDigitsDoubled n = doubleEveryOther $ toDigits n 


parsedNum :: Integer -> [Integer]
parsedNum n = splitDigits $ toDigitsDoubled n

validate :: Integer -> Bool
validate n = 
  if (sumDigits $ parsedNum n) `mod` 10 == 0 then True else False
