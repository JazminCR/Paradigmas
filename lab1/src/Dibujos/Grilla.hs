module Dibujos.Grilla where

import Dibujo (Dibujo, figura, juntar, apilar)
import FloatingPic(Conf(..), Output)
import Graphics.Gloss (text, translate, scale)
import GHC.Float (int2Float)

type Coor = (Int, Int)

-- Convierte una coordenada a un string
coorToString :: Coor -> String
coorToString (x, y) = "(" ++ show y ++ ", " ++ show x ++ ")"  

-- Dibuja una coordenada en la pantalla
interpCoor :: Output Coor
interpCoor (x, y) _ _ _ = translate (int2Float (x*100+25)) (int2Float ((7-y)*100+45)) $ scale 0.13 0.13 $ text (coorToString (x, y))

-- Impresión y configuración

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = row . map column

printCoor :: Dibujo Coor
printCoor = grilla [[ figura (x, y) | x <- [0..7]] | y <- [0..7]]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = printCoor
    , bas = interpCoor
}
