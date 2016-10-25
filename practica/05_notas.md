## Ejercicio 1

> Patterns válidos

**a**. `(x,y)`: válido

**b**. `(1,2)`: válido

**c**. `(n+1)`: inválido

**d**. `(10)`: válido

**e**. `('a',('a',b))`: válido

**f**. `(a,(a,b))`: inválido: aparece dos veces la variable `a`

**g**. `(x,(x,y))`: inválido: aparece dos veces la variable `x`

**h**. `([]:[4])`: válido: [[],4] matcheará. NOP; no pasó el chequeo de tipos. ¿Por qué?

**i**. `(x:y:[])`: válido: matchean las listas con dos elementos

**j**. `[xs]`: válido: listas con un elemento

**k**. `([]:[])`: válido: `[[]]`

## Ejercicio 2

> Definir recursivamente las funciones y dar los tipos

**¿Qué significa decir "*por recursión sobre la lista*"?**

Significa que las listas son un conjunto de valores que se construyó por inducción estructural, y nosotros vamos a aprovechar eso para resolver nuestros problemas.

**`sum`**

```haskell
sum :: [Int] -> Int
-- por recursión sobre la lista
sum [] = 0
sum (x:xs) = x + sum xs
```

**`any`**

```haskell
any :: [Bool] -> Bool
-- por recursión sobre la lista
any [] = False
any (x:xs) = x || any xs
```

**`all`**

```haskell
all :: [Bool] -> Bool
-- por recursión sobre la lista
all [] = True
all (x:xs) = x && all xs
```

**`codes`**

```haskell
codes :: [Char] -> [Int]
-- por recursión sobre la lista
codes [] = []
codes (x:xs) = ord x : codes xs
```

**`remainders`**

```haskell
remainders :: [Int] -> Int -> [Int]
-- por recursión sobre la lista
remainders [] y = []
remainders (x:xs) y = x `mod` y : remainders xs y
```

**`squares`**

```haskell
squares :: [Int] -> [Int]
-- por recursión sobre la lista
squares [] = []
squares (x:xs) = x*x : squares xs
```

**`lengths`**

```haskell
lengths :: [[a]] -> [Int]
-- por recursión sobre la lista
lengths [] = []
lengths (xs:xss) = length xs : lengths xss
```

**`order`**

```haskell
order :: [(Int,Int)] -> [(Int,Int)]
-- por recursión sobre la lista
order [] = []
order ((x,y):xys) = if x < 3*y
						then (x,y) : order xys
						else order xys
```

**`pairs`**

```haskell
pairs :: [Int] -> [Int]
-- por recursión sobre la lista
pairs [] = []
pairs (x:xs) = if even x
						then x : pairs xs
						else pairs xs
```

**`chars`**

```haskell
chars :: [Char] -> [Char]
-- por recursión sobre la lista
chars [] = []
chars (x:xs) = if not (isDigit x)
						then x : chars xs
						else chars xs
```

**`moreThan`**

```haskell
moreThan :: [[a]] -> Int -> [[a]]
-- por recursión sobre la lista
moreThan [] n = []
moreThan (xs:xss) n = if length xs > n
						then xs : moreThan xss n
						else moreThan xss n
```

## Ejercicio 3

> Puede asegurar que las funciones que definió en el ejercicio anterior terminan? ¿Por qué? ¿Qué diferencia encuentra con las demostraciones de la práctica 4?

Sí, puedo porque la recursión se efectúa sobre conjuntos construidos mediante inducción estructural. Esto produce que cualquier elemento inductivo del conjunto se pueda descomponer hasta alcanzar algún elemento correspondiente a las reglas base, y al ser esta descomposición un procedimiento finito, se puede demostrar que las funciones terminan.

Si bien no hice las demostraciones de la práctica 4, da la impresión de que pueden tratarse como recursiones sobre los números naturales, con ciertas modificaciones como por ejemplo que el caso base no necesariamente vaya hasta el elemento base.

## Ejercicio 4

**a**. `[[]] ++ xs = xs`: `xs` es de tipo `[a]`. Falsa: `[]:xs`

**b**. `[[]] ++ [xs] = [[],xs]`: Verdadera

**c**. `[[]] ++ xs = [xs]`: `xs` es de tipo `[a]`. Falsa: `[]:xs`

**d**. `[]:xs = xs`: `xs` es de tipo `[a]`. Falsa: `[]:xs`

**e**. `[[]] ++ [xs] = [xs]`: Falsa: `[[],xs]`

**f**. `[[]] ++ xs = []:xs`: `xs` es de tipo `[a]`. Verdadera

**g**. `[xs] ++ [xs] = [xs,xs]`: Verdadera

**h**. `[] ++ xs = []:xs`: `xs` es de tipo `[a]`. Falsa: `xs`

**i**. `[[]] ++ xs = [[],xs]`: `xs` es de tipo `[a]`. Verdadera

**j**. `[xs] ++ [] = [xs]`: Verdadera

## Ejercicio 5

> Demostrar:

**a**. `pairs . squares = squares . pairs`

**b**. ```((`mod` n) . sum) (remainders n xs) = (sum xs) `mod` n``` ∀n>0

## Ejercicio 6

> Demostrar que `sum xs ≤ len xs * maxl xs`, siendo `xs` una lista finita de números naturales y `maxl`:

```haskell
maxl [] = 0
maxl (x:xs) = x `max` maxl xs
```

```haskell
-- h.i.
sum xs ≤ len xs * maxl xs
-- t.i
sum (x:xs) ≤ len (x:xs) * maxl (x:xs)
-- def. sum.2, len.2, maxl.2
x + sum xs ≤ (1 + len xs) * (x `max` maxl xs)

-- posibilidad 1
x + sum xs ≤ (1 + len xs) * (x `max` maxl xs)
```

## Ejercicio 9

**b**.

```haskell
odds :: [Int] -> [Int]
-- por recursión sobre la lista
odds [] = []
odds (x:xs) = if odd x
				then x : odds xs
				else odds xs
```

## Ejercicio 10

**a**.

```haskell
data DigBin = Cero | Uno deriving Show

suma :: DigBin -> DigBin -> DigBin
suma Cero Cero = Cero
suma Cero Uno = Uno
suma Uno Cero = Uno
suma Uno Uno = Cero

producto :: DigBin -> DigBin -> DigBin
producto Uno Uno = Uno
producto _ _ = Cero
```

**b**.

```haskell
type NumBin = [DigBin]
-- donde la cabeza de la lista representa al dígito menos significativo

carry Uno Uno _ = Uno
carry Uno Cero Uno = Uno
carry Cero Uno Uno = Uno
carry _ _ _ = Cero

sumaB :: NumBin -> NumBin -> NumBin
-- por recursión sobre la lista
sumaB n m = let sumaB' [] [] Cero = []
                sumaB' [] [] Uno = [Uno]
                sumaB' (x:xs) [] c = suma x c : sumaB' xs [] (carry x Cero c)
                sumaB' [] (y:ys) c = suma y c : sumaB' [] ys (carry Cero y c)
                sumaB' (x:xs) (y:ys) c = suma (suma x y) c : sumaB' xs ys (carry x y c)
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
```

**Dudas**:

* `productoPor2` o `productoPor2'`
* ¿Debería poner el caso de `[]` con el error o no?