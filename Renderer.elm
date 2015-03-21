module Renderer where

import Array
import Color (lightBlue, lightGreen)
import Graphics.Collage (collage, path, traced, solid, Path)

import Model

collageWidth  = 400
collageHeight = 400

toCoordinate : (Float, Float) -> (Float, Float)
toCoordinate (x, y) =
    let magX = 400
        magY = 100
        startX = -200
        startY = 0
    in (startX+x*magX, startY+y*magY)

drawString1D : Model.String1D -> Path
drawString1D str =
    let pathify i x = toCoordinate (x, Model.getU str i)
    in path <| Array.toList <| Array.indexedMap pathify str.x

drawString1DQ : Model.String1D -> Path
drawString1DQ str =
    let pathify i x = (x*400-200, (Model.getQ str i)*30)
    in path <| Array.toList <| Array.indexedMap pathify str.x

render str = collage collageWidth collageHeight
             [ traced (solid lightBlue) (drawString1D str)
             , traced (solid lightGreen) (drawString1DQ str)]
