--Exercicio 7

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt


calcula :: ExpInt -> Int
calcula expr = case expr of
    Const n -> n
    Simetrico e -> - (calcula e)
    Mais  e1 e2 -> calcula e1 + calcula e2
    Menos e1 e2 -> calcula e1 - calcula e2
    Mult e1 e2 -> (calcula e1) * (calcula e2)


infixa :: ExpInt -> String
infixa expr = case expr of
    Const n -> show n
    Simetrico e -> "-(" ++ infixa e ++ ")"
    Mais e1 e2 -> "(" ++ infixa e1 ++ " + " ++ infixa e2 ++ ")"
    Menos e1 e2 -> "(" ++ infixa e1 ++ " - " ++ infixa e2 ++ ")"
    Mult e1 e2 -> "(" ++ infixa e1 ++ " * " ++ infixa e2 ++ ")"


posfixa :: ExpInt -> String
posfixa expr = case expr of
    Const n -> show n
    Simetrico e -> posfixa e ++ " - " 
    Mais e1 e2 -> posfixa e1 ++ " " ++ posfixa e2 ++ " + "
    Menos e1 e2 -> posfixa e1 ++ " " ++ posfixa e2 ++ " -"
    Mult e1 e2 ->posfixa e1 ++ " " ++ posfixa e2 ++ " * "


--Exercicio 2

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R n []) = n
soma (R n l) = n + sum (map soma l)


altura :: RTree a -> Int
altura (R n []) = 1
altura (R n l) = 1 + maximum (map altura l)


prune :: Int -> RTree a -> RTree a
prune 1 (R n []) = (R n [])
prune x (R n l) = R n (map (prune (x-1)) l)


mirror :: RTree a -> RTree a
mirror (R n []) = R n []
mirror (R n l) = R n (reverse (map mirror l))


postorder :: RTree a -> [a]
postorder (R n []) = [n]
postorder (R n l) = (concat (map postorder l)) ++ [n]


--Exercicio 3

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)


ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork e d) = ltSum e + ltSum d


listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d


ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork e d) = 1 + (max (ltHeight e)  (ltHeight d))


--Exercicio 4

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)


splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a e d) = (Node a ea da, Fork eb db)
  where
    (ea,eb) = splitFTree e
    (da,db) = splitFTree d


joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip n) = Just (Leaf n)
joinTrees (Node n e d) (Fork e' d') = 
  case (joinTrees e e',joinTrees d d') of
    (Just je, Just jd) -> Just (No n je jd)
    _ -> Nothing
joinTrees _ _ = Nothing 