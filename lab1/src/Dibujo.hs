{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Dibujo
  ( comp,
    figura,
    encimar,
    apilar,
    juntar,
    rot45,
    rotar,
    espejar,
    (^^^),
    (.-.),
    (///),
    r180,
    r270,
    encimar4,
    cuarteto,
    ciclar,
    mapDib,
    change,
    foldDib,
    figuras,
    Dibujo (..),
  )
where

-- nuestro lenguaje

data Dibujo a
  = Figura a
  | Encimar (Dibujo a) (Dibujo a)
  | Apilar Float Float (Dibujo a) (Dibujo a)
  | Juntar Float Float (Dibujo a) (Dibujo a)
  | Rotar (Dibujo a)
  | Rot45 (Dibujo a)
  | Espejar (Dibujo a)
  deriving (Eq, Show)

-- combinadores

infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

-- composición n-veces de una función con sí misma
comp :: Int -> (a -> a) -> a -> a
comp n f
  | n < 0 = error "No se puede componer negativamente"
  | n == 0 = id
  | otherwise = f . comp (n - 1) f

-- funciones constructoras

figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

-- pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- pone un dibujo al lado del otro, ambos ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Juntar 1.0 1.0

-- superpone un dibujo con otro
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Apilar 1.0 1.0

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = Rotar

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 r90

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 r90

-- una figura repetida con las cuatro rotaciones, superimpuestas
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = encimar dib (encimar (rotar dib) (encimar (r180 dib) (encimar (r270 dib) dib))) -- esa dib del último tendría que ser blank como en el Artículo de Henderson D:

-- cuatro figuras en un cuadrante
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto dib1 dib2 dib3 dib4 = apilar 1.0 1.0 (juntar 1.0 1.0 dib1 dib2) (juntar 1.0 1.0 dib3 dib4)

-- un cuarteto donde se repite la imagen, rotada
ciclar :: Dibujo a -> Dibujo a
ciclar dib = cuarteto dib (rotar dib) (r180 dib) (r270 dib)

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura x) = Figura (f x)
mapDib f (Encimar dib1 dib2) = Encimar (mapDib f dib1) (mapDib f dib2)
mapDib f (Apilar n1 n2 dib1 dib2) = Apilar n1 n2 (mapDib f dib1) (mapDib f dib2)
mapDib f (Juntar n1 n2 dib1 dib2) = Juntar n1 n2 (mapDib f dib1) (mapDib f dib2)
mapDib f (Rotar dib) = Rotar (mapDib f dib)
mapDib f (Rot45 dib) = Rot45 (mapDib f dib)
mapDib f (Espejar dib) = Espejar (mapDib f dib)

-- cambiar todas las básicas de acuerdo a la función
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura x) = f x
change f (Encimar dib1 dib2) = Encimar (change f dib1) (change f dib2)
change f (Apilar n1 n2 dib1 dib2) = Apilar n1 n2 (change f dib1) (change f dib2)
change f (Juntar n1 n2 dib1 dib2) = Juntar n1 n2 (change f dib1) (change f dib2)
change f (Rotar dib) = Rotar (change f dib)
change f (Rot45 dib) = Rot45 (change f dib)
change f (Espejar dib) = Espejar (change f dib)

-- fold para nuestro lenguaje
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Figura x) = fatom x
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Espejar dib1) = fesp (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib1)
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Rotar dib1) = frotar (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib1)
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Rot45 dib1) = frot45 (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib1)
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Apilar n1 n2 dib1 dib2) = fapilar n1 n2 (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib1) (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib2)
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Juntar n1 n2 dib1 dib2) = fjuntar n1 n2 (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib1) (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib2)
foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar (Encimar dib1 dib2) = fencimar (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib1) (foldDib fatom fesp frotar frot45 fapilar fjuntar fencimar dib2)

-- Extrae todas las figuras básicas de un dibujo.
figuras :: Dibujo a -> [a]
figuras (Figura x) = [x] -- Si el dibujo es una figura básica, devuelve una lista con ese valor.
figuras (Encimar dib1 dib2) = figuras dib1 ++ figuras dib2 -- Si el dibujo es una composición de dos dibujos, obtiene las figuras de cada uno y las concatena.
figuras (Apilar _ _ dib1 dib2) = figuras dib1 ++ figuras dib2
figuras (Juntar _ _ dib1 dib2) = figuras dib1 ++ figuras dib2
figuras (Rotar dib) = figuras dib -- Si el dibujo está rotado, devuelve las figuras del dibujo original.
figuras (Rot45 dib) = figuras dib
figuras (Espejar dib) = figuras dib
