import Data.List

--Exercicio 1

funcAny :: (a -> Bool) -> [a] -> Bool
funcAny teste [] = True
funcAny teste (h:t) = if (teste h)
    then True
    else funcAny teste t


funcZipWith :: (a->b->c) -> [a] -> [b] -> [c]
funcZipWith zipa l [] = []
funcZipWith zipa [] l = []
funcZipWith zipa (h:t) (x:y) = zipa h x : funcZipWith zipa t y

funcTakeWhile :: (a -> Bool) -> [a] -> [a]
funcTakeWhile f [] = []
funcTakeWhile f (h:t) = if (f h)
    then h : funcTakeWhile f t
    else []


funcDropWhile :: (a -> Bool) -> [a] -> [a]
funcDropWhile f [] = []
funcDropWhile f (h:t) = if (f h)
    then funcDropWhile f t
    else (h:t)


span2 :: (a -> Bool) -> [a] -> ([a],[a])
span2 f [] = ([],[])
span2 f (h:t) | f h = (h:l1,l2)
              | otherwise = (l1,h:l2)
             where (l1,l2) = span2 f t


funcDeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
funcDeleteBy _ _ [] = []
funcDeleteBy f n (x:xs) | f n x = xs
                        | otherwise = x : funcDeleteBy f n xs


funcSortOn :: Ord b => (a -> b) -> [a] -> [a]
funcSortOn f [] = []
funcSortOn f [x] = [x]
funcSortOn f (h:t) = funcInsert f h (funcSortOn f t)


funcInsert :: Ord b => (a->b) -> a -> [a] -> [a]
funcInsert f x [] = [x]
funcInsert f n (x:xs) = if (f n) <= (f x)
    then n:x:xs
    else x : funcInsert f n xs


--Exercicio 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau f x = filter (\(c,e) -> e == f) x

conta :: Int -> Polinomio -> Int
conta n x = foldl (accum n) 0 x
--accum é auxiliar da conta
accum :: Int -> Int -> Monomio -> Int
accum n acc (_,z) = if (n == z) then acc + 1 else acc


conta2 :: Int -> Polinomio -> Int
conta2 n p = length (filter (\(c,e) -> e == n) p)


grau :: Polinomio -> Int
grau [] = error "Nao existe Polinomio"
grau p = maximum (map snd p)


deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (x:xs) = (fromIntegral(snd x)*fst x,(snd x)-1):deriv xs

derivada :: Polinomio -> Polinomio
derivada (h:t) = map deriv (h:t)
  where deriv (a,b) = (a * fromIntegral b, b-1)


deriv2 :: Polinomio -> Polinomio
deriv2 p = let l = map (\(c,g) -> (c * fromIntegral g, g-1)) p
           in filter (\(c,_) -> c /= 0) l


calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x p = sum (map (\(a,b) -> a * x^b) p)


simp :: Polinomio -> Polinomio
simp p = filter (\(a,b) -> a/=0) p


mult :: Monomio -> Polinomio -> Polinomio
mult (0,0) _ = [(0,0)]
mult (x,y) p = map (\(a,b) -> (a*x, b+y)) p


ordena :: Polinomio -> Polinomio
ordena p = funcSortOn snd p


ordena2 :: Polinomio -> Polinomio
ordena2 p = foldr aux [] p
              where
                aux :: Monomio -> Polinomio -> Polinomio
                aux (a,b) [] = [(a,b)]
                aux (a,b) ((c,d):t) | b < d = (a,b):(c,d):t
                                    | otherwise = (c,d) : aux (a,b) t


normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = normaliza (auxNormaliza h t)

auxNormaliza :: Monomio -> Polinomio -> Polinomio
auxNormaliza a [] = []
auxNormaliza (a,b) (h:t) | b == snd h = ((a+fst h), b):t
                         | otherwise = h:(auxNormaliza (a,b) t)


normaliza2 :: Polinomio -> Polinomio
normaliza2 [] = []
normaliza2 ((a,b):t) = let f1 = filter (\(_,b1) -> b == b1) ((a,b):t)
                           f2 = filter (\(_,b1) -> b /= b1) ((a,b):t)
                           x = sum (map fst f1)
                        in (x,b): normaliza2 f2



soma2 :: Polinomio -> Polinomio -> Polinomio
soma2 p1 p2 = foldr somar p1 p2
  where
    somar :: Monomio -> Polinomio -> Polinomio
    somar x [] = [x]
    somar (a,b) ((c,d):t) = if b == d
        then (a+c,b):t
        else (c,d) : somar (a,b) t


produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto p1 p2 = mult (head p1) p2 ++ produto (tail p1) p2


produto2 :: Polinomio -> Polinomio -> Polinomio
produto2 x [] = x
produto2 [] x = x
produto2 p1 p2 = foldr mult p1 p2


equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)


--Exercicio 3

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [] = False
dimOK [x] = True
dimOK (h:i:t) = if length h == length i
    then dimOK (i:t)
    else False


dimOK2 :: Mat a -> Bool
dimOK2 [] = False
dimOK2 [x] = True
dimOK2 (h:t) = all (\l -> length h == length l) t


dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t) | dimOK (h:t) == True = (length h, length (h:t))
             | otherwise = error "Matriz inválida"


addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] m = m
addMat m [] = m
addMat (x:xs) (y:ys) = ((funcZipWith (+)) x y) : (addMat xs ys)


addMat2 :: Num a => Mat a -> Mat a -> Mat a
addMat2 [] m = m
addMat2 m [] = m
addMat2 m1 m2 = zipWith (\l1 l2 -> zipWith (+) l1 l2) m1 m2


transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m = map head m : transpose (map tail m)


multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat _ [] = []
multMat m1 m2 = (map (\x -> (sum (funcZipWith (*) (head m1) x ))) (transpose m2)) : multMat (tail m1) m2


zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat _ [] _ = []
zipWMat _ _ [] = []
zipWMat f (x:xs) (y:ys) = funcZipWith f x y : zipWMat f xs ys


triSup :: (Num a, Eq a) => Mat a -> Bool
triSup [] = False
triSup l = all (==0) (triAux 0 l)
  where
    triAux :: (Num a) => Int -> Mat a -> [a]
    triAux _ [] = []
    triAux x (h:t) = (take x h) ++ triAux (x+1) t


rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft m = if length (head m)>1
    then [(map (last) m)] ++ (rotateLeft (map init m))
    else [(map head m)]


-----------------------------

transpose2 :: Mat a -> Mat a
transpose2 [] = []
transpose2 m = let l1 = map head m
                   l2 = map tail m
               in l1 : transpose l2


zipWMat2 :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat2 f _ [] = []
zipWMat2 f [] _ = []
zipWMat2 f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2


triSup2 :: (Eq a, Num a) => Mat a -> Bool
triSup2 [] = True
triSup2 (h:t) = let l1 = map head t
                    l2 = map tail t
                in (all (==0) l1) && triSup l2


rotateLeft2 :: Mat a -> Mat a
rotateLeft2 ([]:_) = []
rotateLeft2 m = let l1 = map last m
                    l2 = map init m
                in l1 : rotateLeft2 l2
