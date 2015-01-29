module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp{ empFun = f }) (GL l v) = GL (e:l) (f + v)


instance Monoid GuestList where
  mempty                     = GL [] 0 
  mappend (GL x a) (GL xs b) = GL (x ++ xs) (a + b)


moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) 
  | f1 > f2 = g1
  | otherwise = g2


treeFold f b (Node{ subForest = [], rootLabel = a }) = f a b
treeFold f b (Node{ subForest = xs, rootLabel = a } ) = f a $ foldl' (treeFold f) b xs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b ggs = (bestListwithEmp, bestListWithoutEmp) 
  where
    mostFunList :: [GuestList] -> GuestList
    mostFunList = maximumBy compare

    bestListwithEmp :: GuestList
    bestListwithEmp  = mostFunList $ adjustedFun firstList
      where 
        firstList :: [GuestList]
        firstList  = map fst ggs

        adjustedFun :: [GuestList] -> [GuestList]
        adjustedFun = map adjustFun
          where 
            adjustFun :: GuestList -> GuestList
            adjustFun  (GL (x:xs) f) = GL (x:xs) $ f - (empFun x)

    bestListWithoutEmp :: GuestList
    bestListWithoutEmp = mostFunList $ map (glCons b) secondList
      where
        secondList :: [GuestList]
        secondList = map snd ggs

maxFun :: Tree Employee -> GuestList
maxFun Node{ subForest = [], rootLabel = e } = GL [e] 
