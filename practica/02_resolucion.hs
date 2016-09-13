-- 8

data ColorPrimario = Amarillo | Azul | Rojo deriving Show

data ColorSecundario = Naranja | Verde | Violeta deriving Show

-- mezclar :: ColorPrimario -> ColorPrimario -> ColorSecundario
--mezclar cp1 cp2
--    | cp1 == cp2 = error "No se pueden mezclar los mismos colores"
--    | (cp1 == Rojo || cp2 == Rojo) && (cp1 == Azul || cp2 == Azul) = Violeta
--    | (cp1 == Rojo || cp2 == Rojo) && (cp1 == Amarillo || cp2 == Amarillo) = Naranja
--    | (cp1 == Amarillo || cp2 == Amarillo) && (cp1 == Azul || cp2 == Azul) = Verde

mezclar Rojo Azul = Violeta
mezclar Azul Rojo = Violeta
mezclar Rojo Amarillo = Naranja
mezclar Amarillo Rojo = Naranja
mezclar Azul Amarillo = Verde
mezclar Amarillo Azul = Verde
mezclar Rojo Rojo = error "No se mezcla el mismo color"
mezclar Azul Azul = error "No se mezcla el mismo color"
mezclar Amarillo Amarillo = error "No se mezcla el mismo color"

data Punto = PuntoXY Float Float | PuntoXYZ Float Float Float deriving Show

-- modulo :: Punto -> Float
modulo (PuntoXY x y) = sqrt(x^2 + y^2)
modulo (PuntoXYZ x y z) = sqrt(x^2 + y^2 + z^2)

-- distanciaA :: Punto -> Punto -> Float
distanciaA (PuntoXYZ x1 y1 z1) (PuntoXYZ x2 y2 z2) = sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)
distanciaA (PuntoXY x1 y1) (PuntoXY x2 y2)         = distanciaA (PuntoXYZ x1 y1 0) (PuntoXYZ x2 y2 0)
distanciaA (PuntoXY x1 y1) (PuntoXYZ x2 y2 z2)     = distanciaA (PuntoXYZ x1 y1 0) (PuntoXYZ x2 y2 z2)
distanciaA (PuntoXYZ x1 y1 z1) (PuntoXY x2 y2)     = distanciaA (PuntoXYZ x1 y1 z1) (PuntoXYZ x2 y2 0)

xcoord (PuntoXY x y) = x

ycoord (PuntoXY x y) = y

suma (PuntoXYZ x1 y1 z1) (PuntoXYZ x2 y2 z2) = PuntoXYZ (x1 + x2) (y1 + y2) (z1 + z2)
suma (PuntoXY x1 y1) (PuntoXY x2 y2)         = PuntoXY (x1 + x2) (y1 + y2)
suma (PuntoXY x1 y1) (PuntoXYZ x2 y2 z2)     = suma (PuntoXYZ x1 y1 0) (PuntoXYZ x2 y2 z2)
suma (PuntoXYZ x1 y1 z1) (PuntoXY x2 y2)     = suma (PuntoXYZ x1 y1 z1) (PuntoXYZ x2 y2 0)

-- Datos para probar
a = PuntoXY 3 5
b = PuntoXY 10 2
c = PuntoXY (-2) (-5)
d = PuntoXYZ 1 3 7
e = PuntoXYZ 8 2 4
f = PuntoXYZ (-2) (-6) (-4)

data Figura = Circulo Punto Float | Rectangulo Punto Punto Punto Punto deriving Show

-- area :: Figura -> Float
area (Circulo (PuntoXY x y) r) = pi * (r^2)
area (Rectangulo a b c d) = (distanciaA a b) * (distanciaA a d)

perimetro (Circulo (PuntoXY x y) r) = 2 * pi * r
perimetro (Rectangulo a b c d) = (distanciaA a b) + (distanciaA b c) + (distanciaA c d) + (distanciaA a d)

--mover (Circulo xActual yActual r)  = Circulo  r
--mover (Rectangulo aActual b c d) aNuevo = 
--    Rectangulo aNuevo (b + movilizacion) (c + movilizacion) (d + movilizacion)
--    where movilizacion = aNuevo - aActual

-- DUDA: ¿Qué onda? Simplemente no defino área y perímetro para la Figura (Circulo (PuntoXYZ x y z) r)?
-- DUDA: ¿No habría que poder definir en el tipo si va a ser PuntoXY o PuntoXYZ en vez de sólo poner Punto?

data Figura3D = Esfera Figura | Cilindro Figura Float | Cubo Figura Float

-- area :: 
--area3D (Esfera (Circulo (PuntoXYZ x y z) r)) = (4 * pi * (r^3)) / 3
--area3D (Cilindro (Circulo (PuntoXYZ x y z) r) h) = 2 * pi * r * h
--area3D Cubo (Rectangulo (PuntoXYZ x1 y1 z1) (PuntoXYZ x2 y2 z2) (PuntoXYZ x3 y3 z3) (PuntoXYZ x4 y4 z4)) h = 

-- DUDA: ¿Y qué si quiero usar el círculo dentro de la función? ¿Tengo que armarlo?

-- area3D (Esfera circulo) = (4 * pi * (r^3)) / 3 -- No se puede porque necesito conocer r
area3D (Esfera (Circulo (PuntoXYZ x y z) r)) = (4 * pi * (r^3)) / 3
area3D (Cilindro circulo h) = (perimetro circulo) * h
area3D (Cubo rectangulo profundidad) = (area rectangulo) * profundidad
