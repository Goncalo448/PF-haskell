import Data.List

data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | x < y = mdc x (y-x)


normaliza :: Frac -> Frac
normaliza (F a b) = F (signum a * (div a m)) (signum b *(div b m))
  where m = mdc (abs a) (abs b)


instance Eq Frac where
    f1 == f2 = a == c && b == d
      where
        (F a b) = normaliza f1
        (F c d) = normaliza f2


instance Ord Frac where
    compare (F x y) (F w z)
      | x*z > w*y = GT
      | x*z < w*y = LT
      | otherwise = EQ


instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b


instance Num Frac where
    (+) (F a b) (F c d) = normaliza (F (a*d + b*c) (b*d))
    (*) (F a b) (F c d) = normaliza (F (a*c) (b*d))
    (-) (F a b) (F c d) = normaliza (F (a*d - b*c) (b*d))
    abs (F a b) = (F (abs a) (abs b))
    signum (F a b) = (F ((signum a)*(signum b)) 1)
    fromInteger n = (F n 1)


selFrac :: Frac -> [Frac] -> [Frac]
selFrac f l = filter (\fi -> (fi > ((fromInteger 2)*f))) l


--Exercicio 2

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico x) = "- " ++ "(" ++ show x ++ ")"
    show (Mais x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Menos x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")"


calcula :: Num a => Exp a -> a
calcula x = case x of
    Const a -> a
    Simetrico a -> -(calcula a)
    Mais a b -> calcula a + calcula b
    Menos a b -> calcula a - calcula b
    Mult a b -> calcula a * calcula b

instance (Eq a,Num a) => Eq (Exp a) where
    x == y = calcula x == calcula y


instance Num a => Num (Exp a) where
    (+) x y = Const (calcula x + calcula y)
    (-) x y = Const (calcula x - calcula y)
    (*) x y = Const (calcula x * calcula y)
    abs x = Const (abs (calcula x))
    signum x = Const (signum (calcula x))
    fromInteger x = Const (fromInteger x)


--Exercicio 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]


instance Ord Data where
    compare (D d1 m1 a1) (D d2 m2 a2)
      | (d1 > d2 && m1 > m2 || m1 == m2) && a1 == a2 || a1 > a2 = GT
      | d1 == d2 && m1 == m2 && a1 == a2 = EQ
      | otherwise = LT


instance Show Data where
    show (D a m d) = show a ++ "/" ++ show m ++ "/" ++ show d


ordena :: Extracto -> Extracto
ordena (Ext a l) = let l2 = sortOn (\(x,y,z) -> x) l
                   in (Ext a l2)
