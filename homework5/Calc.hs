{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT
import Parser
import qualified StackVM
eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add x1 x2) = eval x1 + eval x2
eval (Mul x1 x2) = eval x1 * eval x2


evalStr :: String -> Maybe Integer
evalStr = parse . readInp 
  where 
    readInp :: String -> Maybe ExprT
    readInp = parseExp Lit Add Mul
    parse :: Maybe ExprT -> Maybe Integer
    parse Nothing = Nothing
    parse (Just x) = Just $ eval x

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  mul a b = Mul a b
  add a b = Add a b

instance Expr Integer where 
  lit n = n
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit n 
    | n <= 0 = False
    | n > 0 = True
  mul a b = a && b
  add a b = a || b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  mul (MinMax a) (MinMax b) = MinMax (max a b)
  add (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 $ n `mod` 7
  add (Mod7 a)  (Mod7 b) = Mod7 $ (a + b) `mod` 7 
  mul (Mod7 a)  (Mod7 b) = Mod7 $ (a * b) `mod` 7 


instance Expr StackVM.Program where
  lit i   = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul
