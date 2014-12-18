{-# OPTIONS_GHC -Wall #-}


module Golf where
  skips :: [a] -> [[a]]
  skips n =  selectEveryNthElement $ indexListTuples n
    where
      indexListTuples :: [a] -> [(Int, [a])]
      indexListTuples  l = zip [x | x <- [1..length l]] $ cycle[l]
      selectEveryNthElement  :: [(Int, [a])] -> [[a]]
      selectEveryNthElement [(i, el)] = map (\(_, a) -> a) $ filteredTuples el 
      filteredTuples :: [a] -> [(Int, a)]
      filteredTuples el = filter parse indexListTuples el 
        where
          parse [(Int)]
