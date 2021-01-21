import Data.Maybe

--Exercicio 1

type MSet a = [(a,Int)]

cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet (x:xs) = snd x + cardMSet xs


moda :: MSet a -> [a]
moda = modaAux 0
  where modaAux :: Int -> MSet a -> [a]
        modaAux _ [] = []
        modaAux n (h:t) | n > snd h = modaAux n t
                        | n < snd h = fst h : modaAux (snd h) t
                        | otherwise = fst h : modaAux n t


converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,b):t) | b == 0 = converteMSet t
                       | otherwise = a : converteMSet ((a,b-1):t)

{-
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies mset a n = foldr (\(x,y) -> (:) (x,n + (if x == a then n else 0))) [] mset
-}

--Exercicio 2

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double  Double
            | Uniao SReais SReais


instance Show SReais where
    show (AA x y) = "]" ++ show x ++ " , " ++ show y ++ "["
    show (FF x y) = "[" ++ show x ++ " , " ++ show y ++ "]"
    show (AF x y) = "]" ++ show x ++ " , " ++ show y ++ "]"
    show (FA x y) = "[" ++ show x ++ " , " ++ show y ++ "["
    show (Uniao a b) = "(" ++ show a ++ " U " ++ show b ++ ")"


pertence :: Double -> SReais -> Bool
pertence n (AA x y) = (n > x) && (n < y)
pertence n (FF x y) = (n >= x) && (n <= y)
pertence n (AF x y) = (n > x) && (n <= y)
pertence n (FA x y) = (n >= x) && (n < y)
pertence n (Uniao a b) = (pertence n a) || (pertence n b)


tira :: Double -> SReais -> SReais
tira n (AA x y) | pertence n (AA x y) = retira n (AA x y)
                | otherwise = (AA x y)
tira n (FF x y) | pertence n (FF x y) = retira n (FF x y)
                | otherwise = (FF x y)
tira n (AF x y) | pertence n (AF x y) = retira n (AF x y)
                | otherwise = (AF x y)
tira n (FA x y) | pertence n (FA x y) = retira n (FA x y)
                | otherwise = (FA x y)
tira n (Uniao a b) | pertence n (Uniao a b) = retira n (Uniao a b)
                   | otherwise = (Uniao a b)

retira :: Double -> SReais -> SReais
retira n (AA x y) | ((x < n) && (y > n)) = (Uniao (AA x n) (AA n y))
retira n (FF x y) | ((x <= n) && (y >= n)) = (Uniao (FA x n) (AF n y))
retira n (AF x y) | ((x < n) && (y >= n)) = (Uniao (AF x n) (FF n y))
retira n (FA x y) | ((x <= n) && (y > n)) = (Uniao (FA x n) (AA n y))
retira n (Uniao a b) = (Uniao (retira n a) (retira n b))


--Exercicio 3

data RTree a = R a [RTree a]

arv :: RTree Int
arv = R 1 [R 3 [R 5 [], R 7 [R 6 [], R 11 []]], R 8 [R 3 [], R 2 []]]


percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R a _) = Just [a]
percorre (x:xs) (R a l) | x > (length l) = Nothing
                        | otherwise = case (percorre xs (l !! (x-1))) of
                                           Just c -> Just (a:c)
                                           Nothing -> Nothing



procura :: Eq a => a -> RTree a -> Maybe [Int]
procura n (R a []) | n == a = Just []
                   | otherwise = Nothing
procura n (R a l) | n == a = Just []
procura n (R a (h:t)) | case (procura n h) of
                            Just c -> Just (1:c)
                            Nothing -> case (procura n (R a t)) of
                                Nothing -> Nothing
                                Just [] -> Just []
                                Just (x:xs) -> Just ((x+1):xs)


