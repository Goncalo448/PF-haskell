--Exercicio 1

encontra :: [a] -> Int -> a
encontra [] n = error "Lista vazia"
encontra (x:xs) 0 = x
encontra (x:xs) n = encontra xs (n-1)


--Exercicio 2

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
    Norte -> posicao (x,y+1) t
    Sul -> posicao (x,y-1) t
    Este -> posicao (x+1,y) t
    Oeste -> posicao (x-1,y) t


funcAny :: (a -> Bool) -> [a] -> Bool
funcAny f [] = False
funcAny f (x:xs) = if f x
    then True
    else funcAny f xs


--Exercicio 4

type Mat a = [[a]]

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (h:t) = let a = map head t
                   b = map tail t
               in (all (==0) a) && triSup b


movimenta :: IO (Int,Int)
movimenta = moveFrom (0,0)

moveFrom :: (Int,Int) -> IO (Int,Int)
moveFrom (x,y) = do
    dir <- getChar
    case dir of
        'n' -> moveFrom (x,y+1)
        's' -> moveFrom (x,y-1)
        'o' -> moveFrom (x-1,y)
        'e' -> moveFrom (x+1,y)
        otherwise -> return (x,y)


--Exercicio 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)])


vazia :: Imagem -> Bool
vazia (Quadrado x) = False
vazia (Mover (_,_) i) = vazia i
vazia (Juntar imgs) = let l2 = map vazia imgs
                      in all (==True) l2


maior :: Imagem -> Maybe Int
maior (Quadrado x) = Just x
maior (Mover (_,_) i) = maior i
maior (Juntar imgs) | null imgs = Nothing
                    | otherwise = maximum (filter (/= Nothing) (map maior imgs))


instance Eq Imagem where
    a == b = null ((quadPos a (0,0)) \\ (quadPos b (0,0)))


quadPos :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quadPos (Quadrado n) pos = [(n,pos)]
quadPos (Mover (a,b) i) (x,y) = quadPos i (x+a,y+b)
quadPos (Juntar imgs) pos = concatMap (\x -> quadPos x pos) imgs
