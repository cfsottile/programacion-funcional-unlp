## Ejercicio 2

```haskell
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
```

## Ejercicio 3

##### ¿Qué es un tipo algebraico?

Muy muy ad-hoc, los tipos algebraicos se basan en la estructura, contrariamente a los tipos abstractos en los que la estructura interna no importa.

> ¿Se pueden representar listas ordenadas mediante tipos algebraicos? Defina o justifique.



## Ejercicio 4

### Dudas

##### ¿Por qué funciona `normSeq`?

...

### Problemas

##### `tailSeq`

Si el árbol tiene la forma

```
       Cat
      /   \
   Cat    (Unit 3)
  /   \
Nil   (Unit 2)
```

la invocación retorna la cola `Cat Nil (Unit 3)`: introduce un `Nil` que antes no estaba.

*¿Es un problema? ¿Cómo solucionarlo?*

###### <u>¿En qué momento se genera?</u>

```haskell
s = (Cat (Cat Nil (Unit 2)) (Unit 3))

tail s
-- ->
tail (Cat s1 s2)
-- ->
app (tail s1) s2
-- ->
Cat (tail s1) s2
-- ->
Cat (tail s2) s2
-- ->
Cat Nil s2
```

`tail s2` resulta en `Nil`. Se podría elegir ignorar `Nil` modificando `tail` en esa línea:

```haskell
tailSeq :: Seq a -> Seq a
-- por recursión sobre la estructura de la secuencia
tailSeq Nil = Nil
tailSeq (Unit _) = Nil
tailSeq (Cat (Unit _) s) = s
tailSeq (Cat s1 s2) =
    if isJust (headSeq s1)
        then if (tailSeq s1) /= Nil
                then appSeq (tailSeq s1) s2
                else s2
        else tailSeq s2
```

pero así estaría descartando también aquellos `Nil` legítimos.

##### `seq2List`

```haskell
seq2List' :: Seq a -> [a]
-- por recursión sobre la estructura de la secuencia
seq2List' Nil = []
seq2List' s =
    if isJust (headSeq s)
        then headSeq s : (seq2List (tailSeq s))
        else seq2List (tailSeq s)
```

No compila:

```
    • Couldn't match type ‘a’ with ‘Maybe a’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          seq2List' :: forall a. Seq a -> [a]
        at 06_resolucion.hs:132:14
      Expected type: [Maybe a]
        Actual type: [a]
```

No estaría bueno poner como tipo `seq2List :: Seq a -> [Maybe a]`. Tampoco lo necesito, porque yo chequeo que sea un `Just`, caso contrario no lo agrego a la lista.

*¿Entonces? ¿Qué hago?*

###### <u>Solución</u>

En 

```haskell
then headSeq s : (seq2List (tailSeq s))
```

en vez de hacer un `cons` de `headSeq s`, hago un `cons` de `fromJust (headSeq s)`.

##### `normSeq`

```haskell
-- normSeq
normSeq1 :: Seq a -> Seq a
-- por recursión sobre la estructura de la lista
normSeq1 Nil = Nil
normSeq1 (Unit x) = Unit x
normSeq1 (Cat s1 s2) = 
    if (normSeq1 s1 == Nil) && (normSeq1 s2 == Nil)
        then Nil
        else if (normSeq1 s1 /= Nil) && (normSeq1 s2 /= Nil)
            then Cat (normSeq1 s1) (normSeq1 s2)
            else if (normSeq1 s1 == Nil)
                then normSeq1 s2
                else normSeq1 s1
```

Da el error

```
06_resolucion.hs:125:9: error:
    • No instance for (Eq a) arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            normSeq1 :: Seq a -> Seq a
    • In the first argument of ‘(&&)’, namely ‘(normSeq1 s1 == Nil)’
      In the expression: (normSeq1 s1 == Nil) && (normSeq1 s2 == Nil)
      In the expression:
        if (normSeq1 s1 == Nil) && (normSeq1 s2 == Nil) then
            Nil
        else
            if (normSeq1 s1 /= Nil) && (normSeq1 s2 /= Nil) then
                Cat (normSeq1 s1) (normSeq1 s2)
            else
                if (normSeq1 s1 == Nil) then normSeq1 s2 else normSeq1 s1
```

*¿Por qué?*