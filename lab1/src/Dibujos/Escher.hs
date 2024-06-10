module Dibujos.Escher where

import Dibujo (Dibujo, figura, juntar, apilar, rotar, rot45, r180, r270, encimar, encimar4, espejar, cuarteto)
import FloatingPic(Conf(..), Output)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (Picture, color, line, violet, withAlpha)


data Color = Violeta | Transparente
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Triangulo
    deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Violeta = color violet
colorear Transparente = color (withAlpha 0.0 violet)

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y, w, (0,0)]

type Escher = (BasicaSinColor, Color)

-- figuras básicas

interpBas :: Output Escher
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

figVioleta :: BasicaSinColor -> Dibujo Escher
figVioleta b = figura (b, Violeta)

blank :: BasicaSinColor -> Dibujo Escher
blank b = figura (b, Transparente)

-- figuras básicas de Escher

trianguloBas :: Dibujo Escher
trianguloBas = figVioleta Triangulo

trianguloSup :: Dibujo Escher
trianguloSup = rot45 trianguloBas

trianguloInf :: Dibujo Escher
trianguloInf = r270 trianguloSup

-- composiciones básicas de Escher

dibujoT:: Dibujo Escher
dibujoT = encimar trianguloBas (encimar trianguloSup trianguloInf)

dibujoU:: Dibujo Escher
dibujoU = encimar4 trianguloSup

lado:: Int -> Dibujo Escher
lado 1 = cuarteto (blank Rectangulo) (blank Rectangulo) (rotar dibujoT) dibujoT
lado 2 = cuarteto (lado 1) (lado 1) (rotar dibujoT) dibujoT
lado n = cuarteto (lado (n-1)) (lado (n-1)) (rotar dibujoT) dibujoT

esquina :: Int -> Dibujo Escher
esquina 1 = cuarteto (blank Rectangulo) (blank Rectangulo) (blank Rectangulo) dibujoU
esquina 2 = cuarteto (esquina 1) (lado 1) (rotar (lado 1)) dibujoU
esquina n = cuarteto (esquina (n-1)) (lado (n-1)) (rotar (lado (n-1))) dibujoU

-- composiciones complejas de Escher

noneto p q r s t u v w x = 
    let row1 = juntar 1 2 p (juntar 1 1 q r)
        row2 = juntar 1 2 s (juntar 1 1 t u)
        row3 = juntar 1 2 v (juntar 1 1 w x)
    in apilar 1 2 row1 (apilar 1 1 row2 row3)

escher :: Int -> Dibujo Escher
escher n = noneto (esquina n) (lado n) (espejar(esquina n))
                  (rotar (lado n)) dibujoU (espejar (rotar (lado n)))
                  (rotar (esquina n)) (r180 (lado n)) (espejar (rotar (esquina n)))

-- impresión y configuración

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

printEscher :: Dibujo Escher
printEscher = grilla [[escher 4]]

escherConf :: Conf
escherConf = Conf {
    name = "Escher"
    , pic = printEscher
    , bas = interpBas
}
