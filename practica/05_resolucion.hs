import Data.Char (ord, isDigit)

-- 2

sum' :: [Int] -> Int
-- por recursión sobre la lista
sum' [] = 0
sum' (x:xs) = x + sum' xs

any' :: [Bool] -> Bool
-- por recursión sobre la lista
any' [] = False
any' (x:xs) = x || any' xs

all' :: [Bool] -> Bool
-- por recursión sobre la lista
all' [] = True
all' (x:xs) = x && all' xs

codes :: [Char] -> [Int]
-- por recursión sobre la lista
codes [] = []
codes (x:xs) = ord x : codes xs

remainders :: [Int] -> Int -> [Int]
-- por recursión sobre la lista
remainders [] y = []
remainders (x:xs) y = x `mod` y : remainders xs y

squares :: [Int] -> [Int]
-- por recursión sobre la lista
squares [] = []
squares (x:xs) = x*x : squares xs

lengths :: [[a]] -> [Int]
-- por recursión sobre la lista
lengths [] = []
lengths (xs:xss) = length xs : lengths xss

order :: [(Int,Int)] -> [(Int,Int)]
-- por recursión sobre la lista
order [] = []
order ((x,y):xys) = if x < 3*y
            then (x,y) : order xys
            else order xys

pairs :: [Int] -> [Int]
-- por recursión sobre la lista
pairs [] = []
pairs (x:xs) = if even x
            then x : pairs xs
            else pairs xs

chars :: [Char] -> [Char]
-- por recursión sobre la lista
chars [] = []
chars (x:xs) = if not (isDigit x)
            then x : chars xs
            else chars xs

moreThan :: [[a]] -> Int -> [[a]]
-- por recursión sobre la lista
moreThan [] n = []
moreThan (xs:xss) n = if length xs > n
            then xs : moreThan xss n
            else moreThan xss n

-- 10

-- a

data DigBin = Cero | Uno deriving Show

suma :: DigBin -> DigBin -> DigBin
suma Cero Cero = Cero
suma Cero Uno = Uno
suma Uno Cero = Uno
suma Uno Uno = Cero

producto :: DigBin -> DigBin -> DigBin
producto Uno Uno = Uno
producto _ _ = Cero

-- b

type NumBin = [DigBin]
-- donde la cabeza de la lista representa al dígito menos significativo

carry3 Uno Uno _ = Uno
carry3 Uno Cero Uno = Uno
carry3 Cero Uno Uno = Uno
carry3 _ _ _ = Cero

carry2 Cero Cero = Cero
carry2 Cero Uno = Cero
carry2 Uno Cero = Cero
carry2 Uno Uno = Uno

sumaB :: NumBin -> NumBin -> NumBin
-- por recursión sobre la lista
sumaB n m = let sumaB' [] [] Cero = []
                sumaB' [] [] Uno = [Uno]
                sumaB' (x:xs) [] c = suma x c : sumaB' xs [] (carry3 x Cero c)
                sumaB' [] (y:ys) c = suma y c : sumaB' [] ys (carry3 Cero y c)
                sumaB' (x:xs) (y:ys) c = suma (suma x y) c : sumaB' xs ys (carry3 x y c)
            in  sumaB' n m Cero

productoPor2 :: NumBin -> NumBin
productoPor2 xs = Cero:xs

productoPor2' :: NumBin -> NumBin
productoPor2' [] = error "[] no es un NumBin válido"
productoPor2' xs = if todosCeros xs then xs else Cero:xs
    where todosCeros [Cero] = True
          todosCeros (Cero:xs) = todosCeros xs
          todosCeros (Uno:xs) = False

cocienteDivisionPor2 :: NumBin -> NumBin
cocienteDivisionPor2 [] = error "[] no es un NumBin válido"
cocienteDivisionPor2 [x] = [Cero]
cocienteDivisionPor2 (x:xs) = xs

restoDivisionPor2 :: NumBin -> DigBin
restoDivisionPor2 [] = error "[] no es un NumBin válido"
restoDivisionPor2 (x:xs) = x

-- c

sumaB''interna :: NumBin -> NumBin -> NumBin
-- por recursión sobre la lista
sumaB''interna [] [] = []
sumaB''interna (x:xs) [] = x : sumaB''interna xs []
sumaB''interna [] (y:ys) = y : sumaB''interna [] ys
sumaB''interna (x:xs) (y:ys) = suma (suma x y) (carry' xs ys) : sumaB''interna xs ys
    where carry' [] [] = Cero 
          carry' (x:xs) (y:ys) = carry2 x y

sumaB'' :: NumBin -> NumBin -> NumBin
sumaB'' (Uno:xs) (Uno:ys) = Uno : sumaB''interna (Uno:xs) (Uno:ys)
sumaB'' xs ys = sumaB''interna xs ys

-- d
digBinAInt :: DigBin -> Int
digBinAInt Cero = 0
digBinAInt Uno = 1

intADigBin :: Int -> DigBin
intADigBin 0 = Cero
intADigBin 1 = Uno

numBinAInt :: NumBin -> Int
-- por recursión sobre la lista
numBinAInt n =
    let f [] i = 0
        f (d:ds) i = ((digBinAInt d) * (2 ^ i)) + (f ds (i+1))
    in  f n 0

intANumBin :: Int -> NumBin
-- por recursión sobre los números naturales
intANumBin 0 = [Cero]
intANumBin 1 = [Uno]
intANumBin n = intADigBin (n `mod` 2) : intANumBin (n `div` 2)

intANumBin' :: Int -> NumBin
intANumBin' n
    | n == 0 || n == 1 = [intADigBin n]
    | n > 1 = intADigBin (n `mod` 2) : intANumBin' (n `div` 2)