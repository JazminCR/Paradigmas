module Interp
  ( interp,
    initial,
    ov,
    r45,
    rot,
    esp,
    sup,
    jun, 
    api
  )
where

import Dibujo (Dibujo (..), foldDib)
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = pictures [p,q]

r45 :: FloatingPic -> FloatingPic
r45 fg d w h = fg (d V.+ 0.5 V.* (w V.+ h)) (0.5 V.*(w V.+ h)) (0.5 V.* (h V.- w))

rot :: FloatingPic -> FloatingPic
rot fg d w h =  fg (d V.+ w) h (V.negate w)
          
esp :: FloatingPic -> FloatingPic
esp fg d w h = fg (d V.+ w) (V.negate w) h

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup p p2 d w h = ov (p d w h) (p2 d w h)

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n fg fg2  d w h= pictures[fg d w' h, fg2 (d V.+ w') (r' V.* w) h ]
                            where
                              w' = m/(m + n) V.* w
                              r' = n/(m + n)
                              
api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api m n fg fg2 d w h = pictures[fg (d V.+ h') w (r V.* h), fg2 d w h']
                      where
                        h' = n/(m+n) V.* h
                        r = m/(m+n)

interp :: Output a -> Output (Dibujo a)
interp f = foldDib f esp rot r45 api jun sup 
