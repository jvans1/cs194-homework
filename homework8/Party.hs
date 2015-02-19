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


funScore :: GuestList -> Fun
funScore (GL _ s) = s
guests :: GuestList -> [Employee]
guests (GL es _) = es

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) 
  | f1 > f2 = g1
  | otherwise = g2


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ls) = f a $ map (treeFold f) ls

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e ggs = (guestListWithBoss, guestListWithoutBoss)
  where 
    guestListWithBoss :: GuestList
    guestListWithBoss = glCons e $ mconcat $ map snd ggs
    guestListWithoutBoss :: GuestList
    guestListWithoutBoss = glCons e $ mconcat $ (map (uncurry moreFun) ggs)


maxFun :: Tree Employee -> GuestList
maxFun empTree = uncurry moreFun $ treeFold nextLevel empTree

main :: IO ()
main = do
  readFile "company.txt" >>= putStrLn . (show . funScore . maxFun . readTree)
  {- putStrLn $ foldr (++) "" $ map nameConvert $ map (\x -> empName x ) $ guests $ maxFun $ readTree empTree -}
  putStrLn "Done"
readTree :: String -> Tree Employee
readTree = read
nameConvert :: Name -> String
nameConvert = id


