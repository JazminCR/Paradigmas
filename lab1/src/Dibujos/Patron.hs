module Dibujos.Patron where

import Dibujo (Dibujo, figura, juntar, apilar, r180, r270, (.-.),  espejar, rotar)
import FloatingPic(Conf(..), Output, half)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (Picture, polygon, color, black, yellow)


data Color = Amarillo | Negro
           deriving (Show, Eq)

data BasicaSinColor = TrianguloRelleno
           deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Amarillo = color yellow
colorear Negro = color black

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor TrianguloRelleno x y w = polygon $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]

type Patron = (BasicaSinColor, Color)

-- figuras básicas

interpBas :: Output Patron
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

trianguloNegro :: Dibujo Patron
trianguloNegro = figura (TrianguloRelleno, Negro)

trianguloAmarillo :: Dibujo Patron
trianguloAmarillo = figura (TrianguloRelleno, Amarillo)

trianguloInf :: Dibujo Patron
trianguloInf = rotar trianguloAmarillo

trianguloSup :: Dibujo Patron
trianguloSup = r270 trianguloNegro

-- composición básica

patronTri :: Dibujo Patron
patronTri  = (.-.) trianguloSup trianguloInf

patronTriDos :: Dibujo Patron
patronTriDos  = r180 (espejar patronTri)

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

printPatron :: Dibujo Patron
printPatron = grilla [[patronTri, patronTri, patronTri, patronTri],
                    [patronTriDos, patronTriDos, patronTriDos, patronTriDos],
                    [patronTri, patronTri, patronTri, patronTri],
                    [patronTriDos, patronTriDos, patronTriDos, patronTriDos],
                    [patronTri, patronTri, patronTri, patronTri],
                    [patronTriDos, patronTriDos, patronTriDos, patronTriDos],
                    [patronTri, patronTri, patronTri, patronTri],
                    [patronTriDos, patronTriDos, patronTriDos, patronTriDos]
                    ]

patronConf :: Conf
patronConf = Conf {
    name = "Patron"
    , pic = printPatron
    , bas = interpBas
}
