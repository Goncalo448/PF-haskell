--Exercicio 1

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:i:t) | h > i = False
                 | h <= i = isSorted (i:t)


inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]


--Exercicio 2

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB = foldl (\x acc -> if maiorQue x acc then x else acc) Nothing
    where maiorQue (Just a) (Just b) = a > b
          maiorQue _ Nothing = True
          maiorQue Nothing _ = False


--Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b


instance Show a => Show (LTree a) where
    show (Tip a) = show a ++ "\n"
    show (Fork a b) = mostra 1 a ++ mostra 1 b

mostra :: (Show a) => Int -> LTree a -> String
mostra n (Tip a) = replicate n '.' ++ show a ++ "\n"
mostra n (Fork a b) = mostra (n+1) a ++ mostra (n+1) b


--Exercicio 4

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (h:t) = maxSumInit' t h h
    where
        maxSumInit' [] _ max = max
        maxSumInit' (h:t) (total) max
          | max > (total+h) = maxSumInit' t (total+h) max
          | otherwise = maxSumInit' t (total+h) (total+h)


--Exercicio 5

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a],a->[a])


convLP :: RelL a -> RelP a
convLP l = concat (map junta l)
    where junta (x,xs) = map (\y -> (x,y)) xs


convPL :: (Eq a) => RelP a -> RelL a
convPL [(x,y)] = [(x,[y])]
convPL (h:t) = junta2 h (convPL t)
    where junta2 (a,b) l = if elem a (map (fst) l)
          then map (\(c,d) -> if c == a then (c,b:d) else (c,d)) l
          else (a,[b]):l


--criaRelPint :: Int -> IO (RelP Int)


convFP :: (Eq a) => RelF a -> RelP a
convFP (a,b) = convLP (map (\x -> (x, b x)) a)


convPF :: (Eq a) => RelP a -> RelF a
convPF x = ((map fst y),f)
    where y = convPL x
          f a = foldl (\acc (b,c) -> if a == b then c else acc) [] y
