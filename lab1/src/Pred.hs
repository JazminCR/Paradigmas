module Pred
  ( Pred,
    cambiar,
    anyDib,
    allDib,
    orP,
    andP,
    falla,
  )
where

import Dibujo (Dibujo (..), foldDib)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = foldDib transformarFigura Espejar Rotar Rot45 Apilar Juntar Encimar
  where
    transformarFigura x
      | p x = f x
      | otherwise = Figura x

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib p (const False) (const False) (const False) compararApilarYJuntar compararApilarYJuntar compararEncimar
  where
    compararApilarYJuntar _ _ d1 d2 = d1 || d2
    compararEncimar d1 d2 = d1 || d2

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib p (const True) (const True) (const True) compararApilarYJuntar compararApilarYJuntar compararEncimar
  where
    compararApilarYJuntar _ _ d1 d2 = d1 && d2
    compararEncimar d1 d2 = d1 && d2

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 x = p1 x && p2 x

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 x = p1 x || p2 x

falla :: Bool
falla = True
