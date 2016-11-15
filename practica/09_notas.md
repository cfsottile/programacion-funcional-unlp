## Ejercicio 1

```haskell
-- 1

data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving Show

foldTip :: (a -> b) -> (b -> b -> b) -> TipTree a -> b
-- por recursión sobre la estructura del árbol
foldTip f1 f2 (Tip x) = f1 x
foldTip f1 f2 (Join t1 t2) = f2 (foldTip f1 f2 t1) (foldTip f1 f2 t2)

heightTip :: TipTree a -> Int
heightTip t = foldTip (const 0) (\n m -> 1 + (max n m)) t

leaves :: TipTree a -> Int
leaves t = foldTip (const 1) (+) t

nodes :: TipTree a -> Int
nodes t = foldTip (const 0) ((+) . (+1)) t

walkover :: TipTree a -> [a]
walkover t = foldTip (:[]) (++) t

mirrorTip :: TipTree a -> TipTree a
mirrorTip t = foldTip Tip (flip Join) t

mapTip :: (a -> b) -> TipTree a -> TipTree b
mapTip f t = foldTip (Tip . f) Join t

-- valor de prueba
t1 = Join (Join (Join (Join (Tip 1)
                            (Tip 2))
                      (Tip 3))
                (Tip 4))
          (Tip 5)
```

## Ejercicio 2

### a, b, c, d

```haskell
-- 2

data BinTree a = Empty | Bin a (BinTree a) (BinTree a) deriving Show

nodesBin :: BinTree a -> Int
-- por recursión sobre la estructura del árbol
nodesBin Empty = 0
nodesBin (Bin x bt1 bt2) = 1 + (nodesBin bt1) + (nodesBin bt2)

heightBin :: BinTree a -> Int
-- por recursión sobre la estructura del árbol
heightBin Empty = 0
heightBin (Bin x bt1 bt2) = 1 + max (heightBin bt1) (heightBin bt2)

mapBin :: (a -> b) -> BinTree a -> BinTree b
-- por recursión sobre la estructura del árbol
mapBin f Empty = Empty
mapBin f (Bin x bt1 bt2) = Bin (f x) (mapBin f bt1) (mapBin f bt2)

mirrorBin :: BinTree a -> BinTree a
-- por recursión sobre la estructura del árbol
mirrorBin Empty = Empty
mirrorBin (Bin x bt1 bt2) = Bin x (mirrorBin bt2) (mirrorBin bt1)

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
-- por re mil recursión sobre la estructura del árbol
-- "donde había un árbol, habrá una recursión sobre ese árbol" frase célebre
foldBin f z Empty = z
foldBin f z (Bin x bt1 bt2) = f x (foldBin f z bt1) (foldBin f z bt2)

mapBin' :: (a -> b) -> BinTree a -> BinTree b
mapBin' = \f -> foldBin (Bin . f) Empty

heightBin' :: BinTree a -> Int
heightBin' = foldBin (\x r1 r2 -> 1 + max r1 r2) 0

mirrorBin' :: BinTree a -> BinTree a
mirrorBin' = foldBin (flip . Bin) Empty

preOrder :: BinTree a -> [a]
preOrder = foldBin (\x r1 r2 -> x:(r1 ++ r2)) []

inOrder :: BinTree a -> [a]
inOrder = foldBin (\x r1 r2 -> r1 ++ [x] ++ r2) []

postOrder :: BinTree a -> [a]
postOrder = foldBin (\x r1 r2 -> r1 ++ r2 ++ [x]) []

-- valores de prueba
bt1 = Empty
bt2 = Bin 1 (Bin 2 Empty (Bin 3 Empty Empty)) Empty
bt3 = Bin 1 Empty Empty
```

### e

##### ¿`mapBin = mapBin'`?

```haskell
mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f Empty = Empty
mapBin f (Bin x bt1 bt2) = Bin (f x) (mapBin f bt1)
									 (mapBin f bt2)
									 
mapBin' :: (a -> b) -> BinTree a -> BinTree b
mapBin' = \f -> foldBin (Bin . f) Empty

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f z Empty = z
foldBin f z (Bin x bt1 bt2) = f x (foldBin f z bt1)
								  (foldBin f z bt2)

¿ mapBin = mapBin' ?
-- por 2 principio de extensionalidad
mapBin f bt = mapBin' f bt -- para todo bt :: BinTree a
-- ¿debería poner para todo f :: (a -> b) también?

-- caso base: bt es Empty
mapBin f Empty = mapBin' f Empty
-- por mapBin.1 y mapBin'
Empty = foldBin (Bin . f) Empty Empty
-- por foldBin.1
Empty = Empty

-- caso inductivo
-- hipótesis inductiva:
mapBin f bt1 = mapBin' f bt1
mapBin f bt2 = mapBin' f bt2
-- tesis inductiva:
mapBin f (Bin x bt1 bt2) = mapBin' f (Bin x bt1 bt2)
-- por mapBin.2 y mapBin'
Bin (f x) (mapBin f bt1) (mapBin f bt2) = foldBin (Bin . f) Empty (Bin x bt1 bt2)
-- por foldBin.2
Bin (f x) (mapBin f bt1) (mapBin f bt2) = (Bin . f) x (foldBin f Empty bt1) (foldBin f Empty bt2)
-- por .
Bin (f x) (mapBin f bt1) (mapBin f bt2) = Bin (f x) (foldBin f Empty bt1) (foldBin f Empty bt2)
-- por h.i.
Bin (f x) (mapBin' f bt1) (mapBin' f bt2) = Bin (f x) (foldBin f Empty bt1) (foldBin f Empty bt2)
-- por mapBin'
Bin (f x) (foldBin (Bin . f) Empty bt1) (foldBin (Bin . f) Empty bt2) = Bin (f x) (foldBin (Bin . f) Empty bt1) (foldBin (Bin . f) Empty bt2)
```

##### ¿`mirrorBin = mirrorBin'`?

```haskell
mirrorBin :: BinTree a -> BinTree a
mirrorBin Empty = Empty
mirrorBin (Bin x bt1 bt2) = Bin x (mirrorBin bt2)
								  (mirrorBin bt1)

mirrorBin' :: BinTree a -> BinTree a
mirrorBin' = foldBin (flip . Bin) Empty

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f z Empty = z
foldBin f z (Bin x bt1 bt2) = f x (foldBin f z bt1)
								  (foldBin f z bt2)

¿ mirrorBin = mirrorBin' ?
-- por principio de extensionalidad
mirrorBin bt = mirrorBin' bt -- para todo bt :: BinTree a
-- por inducción estructural sobre bt

-- caso base: bt es Empty
mirrorBin Empty = mirrorBin' Empty
-- por mirrorBin.1 y mirrorBin'
Empty = foldBin (flip . Bin) Empty Empty
-- por foldBin.1
Empty = Empty

-- caso inductivo
-- hipótesis inductiva:
mirrorBin bt1 = mirrorBin' bt1
mirrorBin bt2 = mirrorBin' bt2
-- tesis inductiva:
mirrorBin (Bin x bt1 bt2) = mirrorBin (Bin x bt1 bt2)
-- por mirrorBin.2 y mirrorBin'
Bin x (mirrorBin bt2) (mirrorBin bt1) = foldBin (flip . Bin) Empty (Bin x bt1 bt2)
-- por foldBin.2
Bin x (mirrorBin bt2) (mirrorBin bt1) = (flip . Bin) x (foldBin (flip . Bin) Empty bt1) (foldBin (flip . Bin) Empty bt2)
-- por .
Bin x (mirrorBin bt2) (mirrorBin bt1) = flip (Bin x) (foldBin (flip . Bin) Empty bt1) (foldBin (flip . Bin) Empty bt2)
-- por flip
Bin x (mirrorBin bt2) (mirrorBin bt1) = Bin x (foldBin (flip . Bin) Empty bt2) (foldBin (flip . Bin) Empty bt1)
-- por mirrorBin <-
Bin x (mirrorBin bt2) (mirrorBin bt1) = Bin x (mirrorBin' bt2) (mirrorBin' bt1)
-- por h.i.
Bin x (mirrorBin bt2) (mirrorBin bt1) = Bin x (mirrorBin bt2) (mirrorBin bt1)
```

### f

##### `mirrorBin . mirrorBin = binTreeId`

```haskell
mirrorBin :: BinTree a -> BinTree a
mirrorBin = foldBin (flip . Bin) Empty

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f z Empty = z
foldBin f z (Bin x bt1 bt2) = f x (foldBin f z bt1)
								  (foldBin f z bt2)

binTreeId :: Bintree a -> BinTree a
binTreeId = id

¿ mirrorBin . mirrorBin = binTreeId ?
-- por principio de extensionalidad, para todo bt :: BinTree a
mirrorBin . mirrorBin bt = binTreeId bt
-- por .
mirrorBin (mirrorBin bt) = binTreeId bt

-- caso base
mirrorBin (mirrorBin Empty) = binTreeId Empty
-- por mirrorBin.1 y binTreeId
mirrorBin Empty = Empty
-- por mirrorBin.1
Empty = Empty

-- caso inductivo
-- hipótesis inductiva:
mirrorBin (mirrorBin bt1) = binTreeId bt1
mirrorBin (mirrorBin bt2) = binTreeId bt2
-- tesis inductiva:
mirrorBin (mirrorBin (Bin x bt1 bt2) = binTreeId (Bin x bt1 bt2)
-- por mirrorBin.2
mirrorBin (foldBin (flip . Bin) Empty (Bin x bt1 bt2))
-- por foldBin.2, (.) y flip
mirrorBin (Bin x (foldBin (flip . Bin) Empty bt2) (foldBin (flip . Bin) Empty bt1)) = binTreeId (Bin x bt1 bt2)
-- por binTreeId
mirrorBin (Bin x (foldBin (flip . Bin) Empty bt2) (foldBin (flip . Bin) Empty bt1)) = Bin x bt1 bt2
-- por mirrorBin <-
mirrorBin (Bin x (mirrorBin bt2) (mirrorBin bt1)) = Bin x bt1 bt2
-- por mirrorBin.2
foldBin (flip . Bin) Empty (Bin x (mirrorBin bt2) (mirrorBin bt1)) = Bin x bt1 bt2
-- por foldBin.2, (.) y flip
Bin x (foldBin (flip . Bin) Empty (mirrorBin bt1)) (foldBin (flip . Bin) Empty (mirrorBin bt2)) = Bin x bt1 bt2
-- por mirrorBin <-
Bin x (mirrorBin (mirrorBin bt1)) (mirrorBin (mirrorBin bt2)) = Bin x bt1 bt2
-- por h.i.
Bin x (binTreeId bt1) (binTreeId bt2) = Bin x bt1 bt2
-- por binTreeId
Bin x bt1 bt2 = Bin x bt1 bt2
```