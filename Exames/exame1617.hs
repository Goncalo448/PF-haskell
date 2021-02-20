--Exercicio 1

funcUnlines :: [String] -> String
funcUnlines [] = ""
funcUnlines [x] = x
funcUnlines (x:xs) = x ++ "\n" ++ funcUnlines xs


remover :: (Eq a) => [a] -> [a] -> [a]
remover [] l = []
remover l [] = l
remover l (x:xs) = remover (funcDelete x l) xs

funcDelete :: Eq a => a -> [a] -> [a]
funcDelete _ [] = []
funcDelete n (x:xs) = if n == x
    then xs
    else x : funcDelete n xs


--Exercicio 2

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

primeiro :: Seq a -> a
primeiro (Inicio a _) = a
primeiro (Fim x y) =  case x of
    Nil -> y
    _ -> primeiro x


semUltimo :: Seq a -> Seq a
semUltimo (Inicio x y) = case y of
    Nil -> Nil
    _ -> Inicio x (semUltimo y)
semUltimo (Fim a b) = a


--Exercicio 3

data BTree a = Empty | Node a (BTree a) (BTree a)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 1 _ = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)


semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d


--Exercicio 4

type Tabuleiro = [String]

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes l = aux l 0 0
  where aux [] _ _ = []
        aux ([]:t) a b = aux t 0 (b+1)
        aux ((x:xs):t) a b = case x of
            '.' -> aux (xs:t) (a+1) b
            'R' -> (a,b) : aux (xs:t) (a+1) b


valido :: Tabuleiro -> Bool
valido = aux . posicoes
       where aux [] = True
             aux (h:t) = (verifica h t) && (aux t)
             verifica _ [] = True
             verifica (c,l) ((c',l'):t) = c /= c' && l /= l' && (c'-c) /= (l'-l) && verifica (c,l) t