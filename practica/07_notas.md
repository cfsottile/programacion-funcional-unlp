## Ejercicio 2

### Duda

* ¿Cuál es la forma de pensar una función? ¿Cómo abordar el diseño de una función? ¿Cómo, a partir de los requerimientos (por ejemplo, de `remDups`, `adjacents` o `primes`), armar una solución?

## Ejercicio 3

```haskell
f = foldr (:) []
```

**a**. El tipo es `[a] -> [a]`, ya que el tipo de `foldr` es `(a->b->b)->b->[a]->b`, y aquí `b` es `[a]` y `foldr` está aplicada con dos argumentos.

**b**. 

**c**.

## Ejercicio 4

```haskell
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = concat (map (\x -> if f x then [x] else []) xs)
```

## Ejercicio 5

```haskell
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
```

**Hacer con foldr y/o recr**

```haskell
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
```

## Ejercicio 6

## Ejercicio 7

No se puede implementar `insert` porque se requiere utilizar x, la recursión sobre xs y también xs, y `foldr` sólo provee x y la recursión sobre xs.

¿`evenPos`?

