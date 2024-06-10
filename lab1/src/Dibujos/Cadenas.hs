module Dibujos.Cadenas where

import Dibujo (Dibujo, figura, apilar, juntar, ciclar)
import FloatingPic(Conf(..), Output, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (Picture, polygon, color, black)

data Color = Negro
           deriving (Show, Eq)

data BasicaSinColor = Random
           deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Negro = color black

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Random x y w = polygon $ map (x V.+) [
        zero, uX,
        uX V.+ 4 V.* uY, uX V.+ 5 V.* uY, x4 V.+ y5,
        x4 V.+ 6 V.* uY, 6 V.* uY, zero
        ]
        where
        uX = (1/6) V.* y
        uY = (1/6) V.* w
        x4 = 4 V.* uX
        y5 = 5 V.* uY

type Cadenas = (BasicaSinColor, Color)

-- figura básica

interpBas :: Output Cadenas
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

principal :: Dibujo Cadenas
principal = figura (Random, Negro)

-- composición básica

abstracta :: Dibujo Cadenas
abstracta = ciclar principal

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

printCadenas :: Dibujo Cadenas
printCadenas = grilla [[abstracta, abstracta, abstracta],
                        [abstracta, abstracta, abstracta],
                        [abstracta, abstracta, abstracta],
                        [abstracta, abstracta, abstracta]
                    ]

cadenasConf :: Conf
cadenasConf = Conf {
        name = "Cadenas"
        , pic = printCadenas
        , bas = interpBas
}
