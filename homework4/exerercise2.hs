data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = foldr buildTree Leaf 
  where
    buildTree :: a -> Tree a -> Tree a
    buildTree x Leaf                  = Node 0 Leaf x Leaf
    buildTree x (Node _ Leaf v   Leaf) = Node 1 (Node 0 Leaf x Leaf) v Leaf
    buildTree x (Node h Leaf v   n  ) = Node h (Node 0 Leaf x Leaf) v n
    buildTree x (Node h n    v Leaf ) = Node h n                    v (Node 0 Leaf x Leaf) 
    buildTree x (Node h n1@(Node h1 _ _ _) v1  n2@(Node h2 _ _ _) ) 
      | h1 > h2     = Node h  n1              v1 (buildTree x n2) 
      | h1 < h2     = Node h (buildTree x n1) v1 n2
      | h1 == h2    = Node (h + 1) (buildTree x n1) v1 n2

    {- buildTree :: a -> Tree a -> Tree a -}
    {- buildTree x Leaf                  = Node 0 Leaf x Leaf -}
    {- buildTree x (Node _ n1 v n2) -}
      {- | height n1 > height n2   = Node (height (buildTree x n2) + 1)    n1            v (buildTree x n2) -}
      {- | height n1 <= height n2  = Node (height (buildTree x n1) + 1) (buildTree x n1) v   n2  -}
        {- where  -}
          {- height :: Tree a -> Integer -}
          {- height Leaf          = -1 -}
          {- height (Node h1 _ _ _) = h1 -}
