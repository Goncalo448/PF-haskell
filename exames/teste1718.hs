import System.Random

--Exercicio 1

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n > x = x : insert n xs
                | n == x = x:n:xs
                | otherwise = n:x:xs


--Exercicio 2

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs


--Exercicio 3

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Var x) = x
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"


--Exercicio 4

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = funcInsert f x (sortOn f xs)


funcInsert :: Ord b => (a -> b) -> a -> [a] -> [a]
funcInsert f n [] = [n]
funcInsert f n (x:xs) = if (f n) <= (f x)
    then n:x:xs
    else x : funcInsert f n xs


--Exercicio 5

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude (x:xs) = aux x 0 xs
  where
    aux :: Int -> Int -> [Int] -> Int
    aux a b [] = b - a
    aux a b (h:t) | h < a = aux h b t
                  | h > a = aux a h t
                  | otherwise = aux a b t

{-
parte :: [Int] -> ([Int],[Int])
-}


--Exercicio 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]


ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])


conta :: Imagem -> Int
conta (Quadrado n) = 1
conta (Mover (a,b) img) = conta img
conta (Juntar []) = 0
conta (Juntar imgs) = sum (map conta imgs)

{-
apaga :: Imagem -> IO Imagem
apaga img =
-}
