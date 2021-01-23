import Data.Char
import Data.List

--Exercicio 1

{-
a) [6,12,18]

b) [6,12,18]

c) [(10,20),(11,19)]

-}

--Exercicio 2

--a) [2^x | x <- [0..10]]

--b) [(x,y) | x <- [0..5], y <- [0..5], x+y == 6]

--c)


--Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x =  (x:ds,ls)
                  | isAlpha x = (ds, x:ls)
                  | otherwise = (ds,ls)
                    where (ds,ls) = digitAlpha xs


--Exercicio 4

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs) | x < 0 = (1+a,b,c)
           | x == 0 = (a,b+1,c)
           | x > 0 = (a,b,c+1)
            where (a,b,c) = nzp xs


--Exercicio 5

divMod' :: Integral a => a -> a -> (a,a)
divMod' x 0 = error "divisão por 0"
divMod' x y | x < 0 && y > 0 || x > 0 && y < 0 = (-res,resp)
            | (x-y) <= 0 = (a,(b+x))
            | otherwise = ((a+1),b)
            where (a,b) = divMod' (x-y) y
                  (res,resp) = divMod' (abs a) (abs b)


maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (h:t) = maxSumInit' (t) (h) (h)

maxSumInit' [] _ max = max
maxSumInit' (h:t) total max
    | max > (total+h) = maxSumInit' (t) (total+h) (max)
    | otherwise = maxSumInit' (t) (total+h) (total+h)


len :: [a] -> Int
len [] = 0
len l = acumulador 0 l
  where
    acumulador :: Int -> [a] -> Int
    acumulador ac [] = ac
    acumulador ac (x:xs) = acumulador (1 + ac) xs


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibo :: Int -> Int
fibo n = acc n (0,1)
  where
    acc 0 (a,c) = a
    acc 1 (a,c) = c
    acc n (a,c) = acc (n-1) (c,a+c)


-----------------------------


divMod2 :: Integral a => a -> a -> (a,a)
divMod2 p s = if p - s > 0
    then (a+1,b)
    else (0,p)
  where (a,b) = divMod2 (p-s) s


fromDigits2 :: [Int] -> Int
fromDigits2 [] = 0
fromDigits2 (h:t) = h*10^(auxDigits t 0) + fromDigits2 t
  where auxDigits [] n = n
        auxDigits (x:xs) n = auxDigits xs (n+1)


maxSumInit2 :: (Num a, Ord a) => [a] -> a
maxSumInit2 [] = error "Lista vazia"
maxSumInit2 (h:t) = auxMSI t h h
  where auxMSI [] _ max = max
        auxMSI (h:t) total max
            | max > (total+h) = auxMSI (t) (total+h) (max)
            | otherwise = auxMSI (t) (total+h) (total+h)


fibonacci :: Int -> Int
fibonacci n = auxFib n (0,1)
  where
    auxFib 0 (a,c) = a
    auxFib 1 (a,c) = c
    auxFib n (a,c) = auxFib (n-1) (c,a+c)
