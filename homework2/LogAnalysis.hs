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
      errorCode el = extractInt el 1

      errorTimeStamp :: String -> Int
      errorTimeStamp el = extractInt el 2 

      extractInt :: String -> Int -> Int
      extractInt el i = read $ (words el)!!i

      errorMessage :: String -> String
      errorMessage el = unwords $ tail $ tail $ tail $ words el

      message :: String -> String
      message el = unwords $ tail $ tail $ words el

      timestamp :: String -> Int
      timestamp el = read $ head $ tail $ words el


  insert :: LogMessage -> MessageTree -> MessageTree
  insert (Unknown _) t = t
  insert lm Leaf = Node Leaf lm Leaf
  insert lm1@(LogMessage _ t _) tree@(Node n1 lm2@(LogMessage _ t2 _) n2)
    | t > t2 = case n1 of  
       Leaf -> Node Leaf lm1 tree 
       _    -> Node (insert lm1 n1) lm2 n2
    | t < t2 = Node n1 lm2 (insert lm1 n2) 

  build :: [LogMessage] -> MessageTree
  build ms = foldr (insert) (Node Leaf (head ms) Leaf) (tail ms)

  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node n1 lm1 n2) = (inOrder n1) ++ [lm1] ++ (inOrder n2)


  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong ms = map extractMessage $ filter isSevere $ orderedLogMessages ms
    where
      orderedLogMessages :: [LogMessage] -> [LogMessage]
      orderedLogMessages lm = inOrder $ build lm 

      extractMessage :: LogMessage -> String
      extractMessage (LogMessage _ _ s) = s
      extractMessage (Unknown _) = "" 

      
      isSevere :: LogMessage -> Bool
      isSevere (LogMessage (Error s) _ _)
        | s >= 50 = True
        | otherwise = False
      isSevere _ = False 
