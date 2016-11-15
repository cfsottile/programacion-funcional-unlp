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

t1 = Join (Join (Join (Join (Tip 1)
                            (Tip 2))
                      (Tip 3))
                (Tip 4))
          (Tip 5)

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

bt1 = Empty
bt2 = Bin 1 (Bin 2 Empty (Bin 3 Empty Empty)) Empty
bt3 = Bin 1 Empty Empty

-- 3

data GenTree a = Gen a [GenTree a]

foldGen :: (a -> [b] -> b) -> GenTree a -> b
foldGen f (Gen x ts) = f x ((map (foldGen f) ts)

foldGen' :: (a -> c -> b) -> ([b] -> c) -> Gen a -> b
foldGen' f g (Gen x ts) = f x (g (map (foldGen' f g) ts))

-- 4

mapGen :: (a -> b) -> GenTree a -> GenTree b
mapGen f = foldGen 