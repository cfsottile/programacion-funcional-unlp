-- 0. Entendimiento de map, filter y foldr

-- filter usando foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr g [] xs
    where g x xs = if f x then x : xs
                          else xs

-- map usando foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs

-- append usando foldr
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)


-- 2

-- primer approach sin funciones de alto orden

-- pal
pal :: [Char] -> Bool
pal s = rev s == s

-- hs
hs1 :: [Char] -> Int
hs1 [] = 0
hs1 (x:xs) = 
    if x == 'h' || (x == 'H')
        then 1 + hs' xs
        else hs' (x:xs)
    where
        -- por recursión sobre la estructura de la lista
        hs' [] = 0
        hs' (' ':'h':xs) = 1 + hs' xs
        hs' (' ':'H':xs) = 1 + hs' xs
        hs' (x:xs) = hs' xs

-- hs2: aaaah, era una lista de palabras, no un string
hs2 :: [[Char]] -> Int
-- por recursión sobre la lista
hs2 [] = 0
hs2 ([]:xss) = hs2 xss
hs2 (xs:xss) =  if (head xs == 'h') || (head xs == 'H')
                    then 1 + hs2 xss
                    else hs2 xss

-- avgLength
avgLength :: [[Char]] -> Int
avgLength [] = 0
avgLength xss = deepLength1 xss `div` (length xss)
    where deepLength1 [] = 0
          deepLength1 (xs:xss) = length xs + (deepLength1 xss)
-- escribir deepLenghtN :: Int -> [a] -> Int

-- adjacents
adjacents :: [a] -> [(a,a)]
-- por recursión sobre la estructura de la lista
adjacents [] = []
adjacents (x:[]) = []
adjacents (x1:x2:xs) = (x1,x2) : (adjacents (x2:xs))

-- diffAdj
diffAdj :: [Int] -> [(Int,Int)]
-- por recursión sobre la estructura de la lista
diffAdj [] = []
diffAdj (x:[]) = []
diffAdj (x1:x2:xs) = 
    if even (x1 - x2)
        then (x1,x2) : (diffAdj (x2:xs))
        else (diffAdj (x2:xs))

-- remDups
remDups :: Eq a => [a] -> [a]
-- por recursión sobre la estructura de la lista
remDups [] = []
remDups (x:[]) = [x]
remDups (x1:x2:xs) = 
    if x1 == x2
        then x1 : (remDups xs)
        else x1 : (remDups (x2:xs))

-- primes
primes :: Int -> [Int]
-- por recursión sobre la estructura de los números
primes 0 = []
primes n = primes (n - 1) ++ [phi2 n]



-- auxiliares
rev xs = fastrev [] xs
    where fastrev xs []     = xs
          fastrev xs (y:ys) = fastrev (y:xs) ys

-- divides
divides a b
    | b < a     = False
    | b == a    = True
    | otherwise = divides a (b-a)

-- prime: dice si un número es primo
prime n = f (n-1) n
    where f 1 n = True
          f x n = (not (x `divides` n)) && (f (x-1) n)

-- pre: i >= 1
-- phi: recibe un i y devuelve el i-ésimo número primo
phi i = f 1 i
    where f n i
            | i == 0      = n
            | prime (n+1) = f (n+1) (i-1)
            | otherwise   = f (n+1) i

phi2 :: Int -> Int
phi2 i = f 1 i
    where
        f n 0 = n
        f n i = if prime (n+1)
                    then f (n+1) (i-1)
                    else f (n+1) i

-- segundo approach con funciones de alto orden

-- pal: es palíndromo?
_pal :: [Char] -> Bool
_pal s = rev s == s

-- hs: cantidad de palabras que empiezan con h
_hs :: [[Char]] -> Int
_hs xss = foldr f 0 xss
    where f x n = if head x == 'h'
                    then 1 + n
                    else n

_hs' xss = length (filter (\x -> head x == 'h') xss)
_hs'' = length . (filter $ \x -> head x == 'h')

-- avgLength: long promedio de palabras en una lista
_avgLength :: [[Char]] -> Int
_avgLength xss = div (sum (map length xss)) (length xss)

__avgLength xss = let p = foldr (\x (s,c)-> (s+x, c+1)) (0,0) xss
                 in fst p / snd p

-- adjacents
_adjacents :: [a] -> [(a,a)]
_adjacents xs = recr f [] xs
    where f x xs recXs = if length xs /= 0
                            then (x, head xs) : recXs
                            else recXs
adj [] = []
adj [x] = []
adj (x:y:xs) = (x,y) : adj (y:xs)                            

_remDups xs = recr f [] xs
    where f x [] recXs = [x]
          f x (x':xs) recXs = if x' == x 
                                then recXs
                                else x : recXs

-- 3

f3 :: [a] -> [a]
f3 = id

-- 4

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = concat (map (\x -> if f x then [x] else []) xs)

-- 5

takewhile :: (a -> Bool) -> [a] -> [a]
-- por recursión sobre la estructura de la lista
takewhile f [] = []
takewhile f (x:xs)
    | f x       = x : takewhile f xs
    | otherwise = []

dropwhile :: (a -> Bool) -> [a] -> [a]
-- por recursión sobre la estructura de la lista
dropwhile f [] = []
dropwhile f (x:xs)
    | f x       = dropwhile f xs
    | otherwise = x : xs

takewhile' :: (a -> Bool) -> [a] -> [a]
takewhile' f xs = foldr (\x xs -> if f x then x : xs else []) [] xs

dropwhile' :: (a -> Bool) -> [a] -> [a]
dropwhile' f xs = recr (\x xs recXs -> if f x then recXs else x : xs) [] xs

takewhile'' :: (a -> Bool) -> [a] -> [a]
takewhile'' f xs = foldr g [] xs
    where g x xs = if f x then x : xs else []

dropwhile'' :: (a -> Bool) -> [a] -> [a]
dropwhile'' f xs = recr g [] xs
    where g x xs recXs = if f x then recXs else x : xs