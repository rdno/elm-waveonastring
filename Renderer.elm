module Renderer where

import Array
import Color (lightBlue, lightGreen, gray, lightRed, lightOrange)
import Graphics.Collage (collage, path, traced, solid, dashed, Path)
import List
import Maybe

import Model

type alias Rectangle = { x0: Float
                       , y0: Float
                       , w: Float
                       , h: Float}

inRectangle : (Float, Float) -> Rectangle -> Bool
inRectangle (x, y) rec =
    let inX = (x <= (rec.x0 + rec.w)) && (x >= rec.x0)
        inY = (y <= (rec.y0 + rec.h)) && (y >= rec.y0)
    in inX && inY

-- interval (min_value, max_value)
type alias Interval = (Float, Float)

-- Computes the length of interval
intervalLen : Interval -> Float
intervalLen (min, max) = max-min

collageRec = { x0 = 0
             , y0 = 0
             , w  = 400
             , h  = 400}

-- Contains information to convert from Data domain to Space domain and back
type alias Converter = { x: Interval
                       , y: Interval
                       , rec: Rectangle }

-- Gives the location of x in UI
spaceX : Converter -> Float -> Float
spaceX c x = c.rec.x0 + x*(c.rec.w/(intervalLen c.x))

-- Gives the location of y in UI
spaceY : Converter -> Float -> Float
spaceY c y = c.rec.y0 + y*(c.rec.h/(intervalLen c.y))

-- Gives the data value of x with stripping UI caling
dataX : Converter -> Float -> Float
dataX c x = (x-c.rec.x0)/(c.rec.w/(intervalLen c.x))

-- Gives the data value of y with stripping UI caling
dataY : Converter -> Float -> Float
dataY c y = (y-c.rec.y0)/(c.rec.h/(intervalLen c.y))


rectU = { x0 = -200
        , y0 = 0
        , w  = 400
        , h  = 200}

converterU = { x   = (0, 1)
             , y   = (-2, 2)
             , rec = rectU}

-- get X value at pos
getX : (Float, Float) -> Float
getX (x, y) = dataX converterU x

rectV = { x0 = -200
        , y0 = 100
        , w  = 400
        , h  = 100}

converterV = { x   = (0, 1)
             , y   = (0, 1)
             , rec = rectV}

-- receiver
rectR = { x0 = -200
        , y0 = -150
        , w  = 400
        , h  = 100}

converterR = { x = (0, 10)
             , y = (-1, 1)
             , rec = rectR}

-- returns the V value for position within limits
getV : (Float, Float) -> Float
getV (x, y) =
    let (ymin, ymax) = converterV.y
        value = dataY converterV y
    in max ymin <| min ymax value

plot : Converter -> List Float -> List Float -> Path
plot conv xs ys =
    let pathify x y = (spaceX conv x, spaceY conv y)
    in path <| List.map2 pathify xs ys

drawString1D : Model.String1D -> Path
drawString1D str =
    let xs = Array.toList <| str.x
        ys = Array.toList <| str.u
    in plot converterU xs ys

drawString1DV : Model.String1D -> Path
drawString1DV str =
    let xs = Array.toList <| str.x
        ys = List.map sqrt <| Array.toList <| str.q
    in plot converterV xs ys

drawBorder : Float -> Path
drawBorder x = path [(spaceX converterU x, -1*collageRec.h/2), (spaceX converterU x, collageRec.h/2)]


drawBorders : Model.String1D -> List Path
drawBorders str = List.map drawBorder str.borders

drawReceiver sim =
    let str = List.head sim.models
        x = Model.getX str sim.receiver
    in path [(spaceX converterU x, spaceY converterU 1), (spaceX converterU x, spaceY converterU -1)]

drawLog sim =
    let str = List.head sim.models
        x = Model.getX str sim.receiver
        getAmp str = Model.getU str sim.receiver
        values = List.reverse <| List.map getAmp sim.models
        n = List.length values
        ts = List.map (\x -> (toFloat x)*sim.dt) [0..n]
    in plot converterR ts values

render sim =
    let str = List.head sim.models
        borderlines = List.map (traced (dashed gray)) <| drawBorders str
    in collage (floor collageRec.w) (floor collageRec.h) <| List.append borderlines [ traced (solid lightBlue) (drawString1D str)
                                                                                    , traced (solid lightGreen) (drawString1DV str)
                                                                                    , traced (solid lightRed) (drawReceiver sim)
                                                                                    , traced (solid lightOrange) (drawLog sim)]

-- Mouse
canvasMousePosition : (Int, Int) -> Maybe (Float, Float)
canvasMousePosition (x, y) =
    let halfW = collageRec.w / 2
        halfH = collageRec.h / 2
        centerX = collageRec.x0 + halfW
        centerY = collageRec.y0 + halfH
        canvasX = (toFloat x)- centerX
        canvasY = centerY - (toFloat y)
        xInCanvas = abs(canvasX) <= halfW
        yInCanvas = abs(canvasY) <= halfH
    in if xInCanvas && yInCanvas then Just (canvasX, canvasY)
       else Nothing

canvasMouseOnV : (Int, Int) -> Bool
canvasMouseOnV pos =
    case canvasMousePosition pos of
      Just coord -> inRectangle coord rectV
      Nothing    -> False

canvasMouseOnU : (Int, Int) -> Bool
canvasMouseOnU pos =
    case canvasMousePosition pos of
      Just coord -> inRectangle coord rectU
      Nothing    -> False
