import Data.Char
import System.Random

--Exercicio 1

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n l
    | n == (last l) = elemIndices n (init l) ++ [(length l)-1]
    | otherwise = elemIndices n (init l)


isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] l = True
isSubsequence l [] = False
isSubsequence (x:xs) (y:ys) = if x == y
    then isSubsequence xs ys
    else isSubsequence (x:xs) ys


--Exercicio 2

data BTree a = Empty | Node a (BTree a) (BTree a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP x (Node (a,b) e d) | x == a = Just b
                            | x < a = lookupAP x e
                            | otherwise = lookupAP x d


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty tree = Empty
zipWithBT f tree Empty = Empty
zipWithBT f (Node r e d) (Node b x y) =
    Node (f r b) (zipWithBT f e x) (zipWithBT f d y)


--Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x = (x:a,b)
                  | isAlpha x = (a,x:b)
                  | otherwise = (a,b)
    where (a,b) = digitAlpha xs


--Exercicio 4

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

ex :: (Show a, Num a) => Seq a
ex = (App (Cons 1 Nil) (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)))

firstSeq ::  Seq a -> a
firstSeq (Cons a s) = a
firstSeq (App Nil s) = firstSeq s
firstSeq (App s _) = firstSeq s


dropSeq :: Int -> Seq a -> Seq a
dropSeq 0 s = s
dropSeq n s = case s of
    Nil -> Nil
    Cons a b -> dropSeq (n-1) b
    App Nil b -> dropSeq (n-1) b
    App s _ -> dropSeq (n-1) s


instance (Show a) => Show (Seq a) where
    show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a b) = show a ++ "," ++ mostra b
mostra (App a b) = mostra a ++ "," ++ mostra b


--Exercicio 5

type Mat a = [[a]]

getElem :: Mat a -> IO a
getElem mat = do
    let (linhas,colunas) = (length mat, length (head mat))
    randLine <- randomRIO (0,linhas-1)
    randRow <- randomRIO (0,colunas-1)
    return ((mat !! randLine) !! randRow)


magic :: Mat Int -> Bool
magic mat = linhasEQ n mat && colunasEQ n mat && diagonaisEQ n mat
    where n = sum (head mat)

linhasEQ :: Int -> Mat Int -> Bool
linhasEQ n mat = foldl (\acc l -> sum l == n && acc) True

colunasEQ :: Int -> Mat Int -> Bool
colunasEQ n mat = foldl (\acc x -> sum (map (\l -> l !! x) mat) == n && acc) True [0..(length mat - 1)]

diagonaisEQ :: Int -> Mat Int -> Bool
diagonaisEQ n mat = sum (map (\n -> (mat !! n) !! n) [0..ln]) == n && sum (map (\n -> (mat !! n) !! (ln - n)) [0..ln]) == n
    where ln = length mat - 1
