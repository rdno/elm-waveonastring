module Renderer where

import Array
import Color (lightBlue, lightGreen, gray)
import Graphics.Collage (collage, path, traced, solid, dashed, Path)
import List

import Model

collageOffsetX = 0
collageOffsetY = 0
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


drawBorder : Float -> Path
drawBorder x = path [(x*400-200, -200), (x*400-200, collageHeight)]


drawBorders : Model.String1D -> List Path
drawBorders str = List.map drawBorder str.borders

render str =
    let borderlines = List.map (traced (dashed gray)) <| drawBorders str
    in collage collageWidth collageHeight <| List.append borderlines [ traced (solid lightBlue) (drawString1D str)
                                                                     , traced (solid lightGreen) (drawString1DQ str)]

inCollage : (Int, Int) -> Bool
inCollage (x, y) = (collageWidth + collageOffsetX >= x) && (collageHeight + collageOffsetY >= y)

layerRelativeY : (Int, Int) -> Float
layerRelativeY (x, y) = (200 - (toFloat y))/30

asX : (Int, Int) -> Float
asX (x, y) = 1/400 * (toFloat x)
