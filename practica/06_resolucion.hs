-- 2
data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving Show
-- árbol de prueba
t1 = Join (Join (Join (Join (Tip 1)
                            (Tip 2))
                      (Tip 3))
                (Tip 4))
          (Tip 5)

-- heightTip: longitud del camino más largo desde la raíz hasta una hoja
heightTip :: TipTree a -> Int
-- por recursión sobre el árbol
heightTip (Tip x) = 0
heightTip (Join t1 t2) = 1 + (max (heightTip t1) (heightTip t2))

-- leaves: número de hojas
leaves :: TipTree a -> Int
-- por recursión sobre el árbol
leaves (Tip x) = 1
leaves (Join t1 t2) = leaves t1 + leaves t2

-- nodes: número de nodos que no son hojas
nodes :: TipTree a -> Int
-- por recursión sobre el árbol
nodes (Tip x) = 0
nodes (Join t1 t2) = 1 + nodes t1 + nodes t2

-- walkover: lista de las hojas, leídas de izquierda a derecha
walkover :: TipTree a -> [a]
-- por recursión sobre el árbol
walkover (Tip x) = [x]
walkover (Join t1 t2) = walkover t1 ++ walkover t2

-- mirrorTip: imagen especular del árbol: intercambio entre subárboles izq y
-- der en cada nodo
mirrorTip :: TipTree a -> TipTree a
-- por recursión sobre el árbol
mirrorTip (Tip x) = Tip x
mirrorTip (Join t1 t2) = Join (mirrorTip t2) (mirrorTip t1)

-- mapTip: toma una función y un árbol; retorna el árbol resultante de aplicar
-- la función sobre cada nodo (¿sobre cada elemento sería? ¿sobre cada hoja?)
mapTip :: (a -> b) -> TipTree a -> TipTree b
-- por recursión sobre el árbol
mapTip f (Tip x) = Tip (f x)
mapTip f (Join t1 t2) = Join (mapTip f t1) (mapTip f t2)

-- 4

data Seq a = Nil | Unit a | Cat (Seq a) (Seq a) deriving Show

-- appSeq: concatenación de dos secuencias
appSeq :: Seq a -> Seq a -> Seq a
appSeq s1 s2 = Cat s1 s2
-- ¿appSeq = Cat?

-- conSeq: toma un elemento y una secuencia y retorna una secuencia
conSeq :: a -> Seq a -> Seq a
conSeq x s = Cat (Unit x) s

-- lenSeq
lenSeq :: Seq a -> Int
-- por recursión sobre la secuencia
lenSeq Nil = 0
lenSeq (Unit x) = 1
lenSeq (Cat s1 s2) = lenSeq s1 + lenSeq s2

-- revSeq
revSeq :: Seq a -> Seq a
-- por recursión sobre la lista
revSeq Nil = Nil
revSeq (Unit x) = Unit x
revSeq (Cat s1 s2) = Cat (revSeq s2) (revSeq s1)

revSeq' :: Seq a -> Seq a
-- por recursión sobre la lista
revSeq' (Cat s1 s2) = Cat (revSeq' s2) (revSeq' s1)
revSeq' s = s

-- headSeq
headSeq :: Seq a -> a
-- por recursión sobre la lista
headSeq (Unit x) = x
headSeq (Cat s1 s2) = headSeq s1

-- tailSeq
tailSeq :: Seq a -> Seq a
tailSeq (Cat s1 s2) = s2
tailSeq _ = Nil

-- normSeq: retorna el árbol recibido sin los Nil
normSeq :: Seq a -> Seq a
-- por recursión sobre la secuencia
normSeq Nil = Nil
normSeq (Unit x) = (Unit x)
normSeq (Cat Nil Nil) = Nil
normSeq (Cat Nil s) = normSeq s
normSeq (Cat s Nil) = normSeq s
normSeq (Cat s1 s2) = normSeq (Cat (normSeq s1) (normSeq s2))

-- eqSeq
eqSeq :: Eq a => Seq a -> Seq a -> Bool
-- por recursión sobre la lista
eqSeq Nil Nil = True
eqSeq (Unit x) (Unit y) = x == y
eqSeq (Cat s11 s12) (Cat s21 s22) = (eqSeq s11 s21) && (eqSeq s12 s22)
eqSeq _ _ = False

-- seq2List
seq2List :: Seq a -> [a]
-- por recursión sobre la secuencia
seq2List Nil = []
seq2List (Unit x) = [x]
seq2List (Cat s1 s2) = seq2List s1 ++ seq2List s2

-- 5
data Form = Atom                | Not Form 
          | Or Form Form        | And Form Form
          | Implies Form Form   | Iff Form Form
          | Forall Char Form    | Exists Char Form
          deriving Show

normalize :: Form -> Form
-- por recursión sobre la fórmula
normalize Atom = Atom
normalize (Not f) = Not (normalize f)
normalize (Or f1 f2) = Or (normalize f1) (normalize f2)
normalize (And f1 f2) = Not (Or (Not (normalize f1)) (Not (normalize f2)))
normalize (Implies f1 f2) = Or (Not (normalize f1)) (normalize f2)
normalize (Iff f1 f2) = Not (Or (Not (Or (Not (normalize f1)) (normalize f2))) (Not (Or (Not (normalize f2)) (normalize f1))))
normalize (Forall x f) = Not (Exists x (Not (normalize f)))
normalize (Exists x f) = Exists x (normalize f)

data FN = FNAtom | FNNot FN | FNOr FN FN | FNExists Char FN deriving Show

fn2FN :: Form -> FN
-- por recursión sobre la fórmula
fn2FN Atom = FNAtom
fn2FN (Not f) = FNNot (fn2FN f)
fn2FN (Or f1 f2) = FNOr (fn2FN f1) (fn2FN f2)
fn2FN (Exists x f) = FNExists x (fn2FN f)
fn2FN _ = error "la fórmula no se encontraba en forma normal"

form2FN :: Form -> FN
form2FN f = fn2FN (normalize f)