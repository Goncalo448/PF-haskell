import Data.Char

--Exercício 1

--a)

perimetro :: Double -> Double
perimetro x = (2 * pi * x)

--b)

distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2))

--c)

primUlt :: [a] -> (a, a)
primUlt x = (head x,  last x)

--d)

multiplo m n = do 
    if m `mod` n == 0
        then print(show(m) ++ " is multiple of " ++ show(n))
    else
        print(show(m) ++ " is not multiple of " ++ show(n))

--e)

truncaImpar :: [a] -> [a]
truncaImpar x = if (length x) `mod` 2 == 0
  then x
  else tail x 

--f)

max2 :: Int -> Int -> Int
max2 x y = if (x > y) then x else y 

--g)

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z


--Exercício 2


--a)

nRaizes :: Double -> Double -> Double -> Double
nRaizes a b c = if determinante >= 0
  then 2
  else if determinante == 0
    then 1
    else 0
    where
      determinante = (b^2 + 4 * a * c)

--b)

raizes :: Double -> Double -> Double -> [Double]
raizes a b c 
  | nRaizes a b c == 2 = [(-b + root) / 2 * a, (-b - root) / 2 * a]
  | nRaizes a b c == 1 = [-b / 2 * a]
  | otherwise = []
  where
    root = sqrt (b^2 - 4 * a * c)


--Exercício 3

--a)

type Hora = (Int, Int)

valida :: Hora -> Bool
valida (h, m) = h > 0 && h <= 23 && m >= 0 && m <= 59

--b)

depoisDe :: Hora -> Hora -> Bool
depoisDe (h1, m1) (h2, m2) = (h1 > h2) || (h1 == h2 && m1 > m2)

--c)

hora2min :: Hora -> Int
hora2min (h, m) = h * 60 + m 

--d)

min2hora :: Int -> Hora
min2hora minutos = (h, m) where
   h = mod (div minutos 60) 24
   m = mod minutos 60

--e)

difHora :: Hora -> Hora -> Int
difHora h1 h2 = abs ((hora2min h1) - (hora2min h2))

--f)

addMin :: Hora -> Int -> Hora
addMin h m = min2hora ((hora2min h) + m)


--Exercício 4

data Tempo = T Int Int deriving (Show,Eq)

--a)
horaValida :: Tempo -> Bool
horaValida (T h m) = valida(h, m)

--b)
depois :: Tempo -> Tempo -> Bool
depois (T h1 m1) (T h2 m2) = depoisDe (h1, m1) (h2, m2)

--c)
horaConvMin :: Tempo -> Int
horaConvMin (T h m) = hora2min (h, m)

--d)
minConvHora :: Int -> Tempo
minConvHora min = (T h m) where
   h = mod (div min 60) 24
   m = mod min 60

--e)
difer :: Tempo -> Tempo -> Int
difer (T h1 m1) (T h2 m2) = abs (horaConvMin (T h1 m1) - horaConvMin (T h2 m2))

--f)
sumMin :: Tempo -> Int -> Tempo
sumMin (T h m) min = minConvHora (horaConvMin (T h m) + min)


--Exercício 5

--a)
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next s = case s of
    Verde -> Amarelo
    Amarelo -> Vermelho
    Vermelho -> Verde

--b)

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = stop s1 || stop s1


--Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--a)
posx :: Ponto -> Double
posx a = case a of
    Cartesiano x y -> x
    Polar r a -> r * cos a

--b)
posy :: Ponto -> Double
posy b = case b of
    Cartesiano x y -> y
    Polar r a -> r * sin a

--c)
raio :: Ponto -> Double
raio a = case a of
    Cartesiano x y -> sqrt (x^2 + y^2)
    Polar r a -> r

--d)
angulo :: Ponto -> Double
angulo a = case a of
    Cartesiano x y -> atan (y / x)
    Polar r a -> a

--e)
dist' :: Ponto -> Ponto -> Double
dist' p1 p2 = distancia (posx p1, posy p1) (posx p2, posy p2)


--Exercício 7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

--a)
poligono :: Figura -> Bool
poligono (Circulo p1 r) = False
poligono _ = True

--b)
vertice :: Figura -> [Ponto]
vertice (Triangulo p1 p2 p3) = [p1,p2,p3]
vertice (Circulo _ _) = []
vertice (Retangulo a1 a2) = [a1,a2,a3,a4] where
    a3 = Cartesiano (posx a1) (posy a2)
    a4 = Cartesiano (posx a2) (posy a1)

--c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist' p1 p2
        b = dist' p2 p3
        c = dist' p3 p1
        s = (a + b + c) / 2 -- semi perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c))

area (Circulo p r) = pi * r^2
area (Retangulo p1 p2) = base * altura where
    p4 = (Cartesiano (posx p2) (posy p1))
    base = dist' p1 p4
    altura = dist' p2 p4

--d)
perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = perimetro r
perimetro' (Retangulo p1 p2) = 2*base + 2*altura where
    p4 = (Cartesiano (posx p2) (posy p1))
    base = dist' p1 p4
    altura = dist' p2 p4
perimetro' (Triangulo p1 p2 p3) =
    let a = dist' p1 p2
        b = dist' p2 p3
        c = dist' p3 p1
    in (a+b+c)

--Exercicio 8

--a)
isLower' :: Char -> Bool
isLower' x = do
    let b = ['a'..'z']
    x `elem` b

--b)
isDigit' :: Char -> Bool
isDigit' k = do
    let c = [48..57]
    ord k `elem` c

--c)
isAlpha' :: Char -> Bool
isAlpha' y = do
    let d = [65..90]
    let a = [97..122]
    ord y `elem` a || ord y `elem` d

--d)
toUpper' :: Char -> Char
toUpper' l = do
    let c = isLower' l
    let w = ord l 
    if c == True
        then chr(w-32)
        else l 

--e)
intToDigit' :: Int -> Char
intToDigit' x = chr(48 + x)

--f)
digitToInt' :: Char -> Int
digitToInt' a = do
    let b = ord a
    let c = b-48
    c

