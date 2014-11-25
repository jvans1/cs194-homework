module LogAnalysis where
  import Log
  parse :: String -> [LogMessage]
  parse l = map parseMessage $ lines l 
  parseMessage :: String -> LogMessage
  parseMessage l
    | head l == 'E' = LogMessage (Error $ errorCode l) (errorTimeStamp l) (errorMessage l)
    | head l == 'I' = LogMessage Info (timestamp l) (message l)
    | head l == 'W' = LogMessage Warning (timestamp l) (message l)
    | otherwise = Unknown l
    where 
      errorCode :: String -> Int
      errorCode l = extractInt l 1

      errorTimeStamp :: String -> Int
      errorTimeStamp l = extractInt l 2 

      extractInt :: String -> Int -> Int
      extractInt l i = read $ (words l)!!i

      errorMessage :: String -> String
      errorMessage l = unwords $ tail $ tail $ tail $ words l

      message :: String -> String
      message l = unwords $ tail $ tail $ words l

      timestamp :: String -> Int
      timestamp l = read $ head $ tail $ words l


  insert :: LogMessage -> MessageTree -> MessageTree
  insert m Leaf = Node Leaf m Leaf
  insert m MessageTree = Node Leaf m Leaf
