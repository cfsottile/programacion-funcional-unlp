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

##### b

¿Ventajas y Desventajas?

##### Dudas

* ¿`headSeq Nil` es `Nil`?
* ¿Por qué funciona `normSeq`?