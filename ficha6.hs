--Exercicio 1

data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = 1 + (max (altura e) (altura d))


contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r Empty Empty) = 1
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d


folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d


prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 1 _ = Empty
prune p (Node r e d) = Node r (prune (p-1) e) (prune (p-1) d)


path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path (x:xs) (Node r e d) | x == True = r : (path xs d)
                         | otherwise = r : (path xs e)
path [] x = []


mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r Empty Empty) = (Node r Empty Empty)
mirror (Node r e d) = (Node r (mirror d) (mirror e))


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f Empty _ = Empty
zipWithBT f (Node r e d) (Node a b c) = Node (f r a) (zipWithBT f e b) (zipWithBT f d c)


unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a ea da, Node b eb db, Node c ec dc)
  where (ea,eb,ec) = unzipBT e
        (da,db,dc) = unzipBT d


--Exercicio 2

minimo :: Ord a => BTree a -> a
minimo (Node r Empty Empty) = r
minimo (Node r Empty d) = minimo d
minimo (Node r e d) = minimo e


semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty Empty) = Empty
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d


minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node r Empty Empty) = (r, Empty)
minSmin (Node r e d) = (c, Node r b Empty)
  where (c,b) = minSmin e


remove :: Ord a => a -> BTree a -> BTree a
remove n Empty = Empty
remove n (Node r e d) | n <= r = remove n e
                      | n >= r = remove n d
                      | otherwise = (Node r (remove n e) (remove n d))

--Exercicio 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = Ord | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou

   deriving Show
type Turma = BTree Aluno --arvore binaria de procura


inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (x,_,_,_) e d) | n == x = True
                               | n < x = inscNum n e
                               | otherwise = inscNum n d


inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_,x,_,_) e d) | n == x = True
                                | otherwise = inscNome n e || inscNome n d


trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (x,n,_,_) e d) = [(x,n)] ++ trabEst e ++ trabEst d


nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing
nota x (Node (a,b,r,c) e d) | x == a = Just c
                            | x > a = nota x d
                            | otherwise = nota x e


percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas a = (faltas a) / fromIntegral (contaNodos a)
  where faltas :: Turma -> Float
        faltas Empty = 0
        faltas (Node (_,_,_,Faltou) e d) = 1 + faltas e + faltas d
        faltas (Node (_,_,_,x) e d) = faltas e + faltas d


mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov a = (soma a) / fromIntegral (contaNodos a)
  where soma :: Turma -> Float
        soma Empty = 0
        soma (Node (_,_,_,Aprov x) e d) = (fromIntegral x) + soma e + soma d
        soma (Node (_,_,_,_) e d) = soma e + soma d


aprovAV :: Turma -> Float
aprovAV Empty = 0
aprovAV turma = (fromIntegral (aux1 turma)) / fromIntegral (contaNodos turma)
  where aux1 :: Turma -> Int
        aux1 (Node (_,_,_,Aprov n) e d) = 1 + aux1 e + aux1 d
        aux1 (Node (_,_,_,_) e d) = aux1 e + aux1 d
