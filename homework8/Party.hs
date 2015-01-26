module Party where
  import Data.Monoid
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
