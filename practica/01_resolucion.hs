-- 1

--seven :: Int -> Int
seven x = 7
-- ¿Por qué?
-- >>>> :t seven
-- >>>> seven :: Num t1 => t -> t1

--sign :: Int -> Int
sign x 
    | x == 0 = 0
    | x > 0 = 1
    | x < 0 = -1

absolute x
    | x >= 0 = x
    | x < 0 = x * (-1)

absolute' x = x * sign x

and' a b
    | a == True = b
    | otherwise = False

-- DUDA: Las tuplas, ¿se escriben con un espacio luego de la coma? Es decir, ¿(a,b) o (a, b)?
-- RESPUESTA: ghci lo escribe sin espacio, así que...
-- DUDA: ¿Por qué hacerlo con una tupla cuando podemos usar dos parámetros? Más aún, el
-- enunciado especifica "toma dos números"
and'' (a,b)
    | a == True = b
    | otherwise = False

or' a b
    | a == False = b
    | otherwise = True

or'' (a, b)
    | a == False = b
    | otherwise = True

not' a
    | a == True = False
    | a == False = True

xor' a b
  | a /= b = True
  | otherwise = False

divides a b = b `mod` a == 0

isMultipleOf a b = b `divides` a
-- PROBLEMA: La función no puede retornar un resultado válido si recibe los argumentos 40 y 0.
-- ¿Debería dejarlo así? ¿O debería corroborar que no recibí un 0 como segundo argumento?
-- El problema se repite en las funciones isCommonDivisor e isCommonMult

isCommonDivisor a b c = a `divides` b && a `divides` c
-- DUDA: ¿Por qué no es necesario desambiguar con paréntesis?

isCommonMult a (b,c) = a `isMultipleOf` b && a `isMultipleOf` c

swap (a,b) = (b,a)

-- 2

-- a
--f x =
--    let (y,z) = (x,x)
--    in y

f x = x

-- b
--greaterThan (x,y) = if x > y then True else False

greaterThan (x,y) = x > y

-- c
--f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b

g (x,y) = x

-- 3. Redefinir la función `power4` de dos formas diferentes

--power4 x =
--    let sqr y = y * y
--    in sqr (sqr x)

power4 x = sqr (sqr x)
    where sqr y = y * y

power4' x = x^4

-- 4. Definir fib

--fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--fib' :: Int -> Int
fib' n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

-- 5. Enumerar propiedades deseables en los programas

-- * Terminación
-- * Corrección
-- * Claridad
-- * Simplicidad
-- * Generalidad
-- * Eficiencia
-- * Modificabilidad

-- 6. Según tu apreciación, ¿qué caracteriza al paradigma funcional?

-- Según entiendo, la característica principal del paradigma funcional es que
-- se basa en definiciones de funciones de naturaleza matemática, lo cual
-- permite que exista una gran facilidad de transmisión de ideas abstractas y
-- con cierto sustento matemático a programas concretos, sin que se pierda la
-- claridad y robustez matemática.

-- 7. Probar el código de los ejercicios en un intérprete

-- 8. Implementar una función `random` que, dado un intervalo, retorne un
-- número dentro del mismo de forma aleatoria.

-- La implementación de una función random no es trivial, ya que, por su
-- naturaleza, las funciones, a partir de un valor, retornan siempre el mismo
-- valor. Aparentemente, los conocimientos son algo más avanzados de los que
-- tenemos hasta el momento. Los siguientes links proveen información útil: 
--
-- * How do functional languages handle random numbers?: un usuario de
-- stackoverflow explica cómo se podría implementar la función `random`
-- [link](http://programmers.stackexchange.com/q/202908)
-- * System.Random
-- [link](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
-- * Random numbers in Haskell
-- [link](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)

-- 9. 

esBisiesto y = ((d /= 0) && (4 `divides` d)) || (400 `divides` y)
    where d = decenas y

esBisiesto' y
    | d /= 0 && 4 `divides` d = True
    | 400 `divides` y = True
    | otherwise = False
    where d = decenas y

esBisiesto'' y = (d /= 0 && 4 `divides` d) || 400 `divides` y
    where d = decenas y

esBisiesto''' y =
    let d = decenas y
    in (d /= 0 && 4 `divides` d) || 400 `divides` y

decenas n = n `mod` 100

-- 10.

sort3 a b c
    | (a <= b) && (b <= c) = (a,b,c)
    | (a <= c) && (c <= b) = (a,c,b)
    | (b <= a) && (a <= c) = (b,a,c)
    | (b <= c) && (c <= a) = (b,c,a)
    | (c <= a) && (a <= b) = (c,a,b)
    | otherwise = (c,b,a)
    -- | (c <= b) && (b <= a) = (c,b,a)