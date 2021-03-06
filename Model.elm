module Model where

import Array
import List
import Maybe

import Utils


type Boundary = Fixed | Loose | None

type alias String1D = { x : Array.Array Float
                      , borders: List Float
                      , layers: List Float
                      , q : Array.Array Float
                      , u : Array.Array Float
                      , u_old: Array.Array Float
                      , t : Float
                        -- source t x = ...
                      , source : (Float -> Float -> Float)
                      , left :  Boundary
                      , right : Boundary}

type alias Neighbors = (Float, Float, Float)

getOrZero : Int -> Array.Array Float -> Float
getOrZero index arr = Maybe.withDefault 0 <| Array.get index arr

getX : String1D -> Int -> Float
getX str i = getOrZero i str.x

getDx : String1D -> Float
getDx str = (getX str 1) - (getX str 0)

getXID : String1D -> Float -> Int
getXID str x = floor (x / (getDx str))

getU : String1D -> Int -> Float
getU str i = getOrZero i str.u

getUold : String1D -> Int -> Float
getUold str i = getOrZero i str.u_old

getQ : String1D -> Int -> Float
getQ str i = getOrZero i str.q

getNeighbors : Array.Array Float -> Int -> Neighbors
getNeighbors arr i = ( getOrZero (i-1) arr
                     , getOrZero (i) arr
                     , getOrZero (i+1) arr)

finiteDifference : String1D -> Int -> Float -> Float
finiteDifference str i dt =
    let dx = getDx str
        c = (dx/dt)^2
        (lu, mu, ru) = getNeighbors str.u i
        uo = getUold str i
        x = getX str i
        f = str.source (str.t+dt) x
        (lq, mq, rq) = getNeighbors str.q i
    in -1*uo + 2*mu + 0.5*c*((mq+rq)*(ru-mu) - (mq+lq)*(mu-lu)) + dt^2*f

step : String1D -> Float -> String1D
step str dt =
    let dx = getDx str
        last = (Array.length str.x) - 1
        nt = str.t + dt
        u0 nstr =
            case nstr.left of
              Fixed -> 0
              Loose -> getU nstr 1
              None -> getUold nstr 1
        uL nstr =
            case nstr.right of
              Fixed -> 0
              Loose -> getU nstr (last-1)
              None -> getUold nstr (last-2)
        stepNode i _ =
            if | i == 0    ->  u0 str
               | i == last ->  uL str
               | otherwise ->  finiteDifference str i dt
        -- Apply Boundary conditions
        bc nstr =
            let set_u0 = Array.set 0 (u0 nstr)
                set_uL = Array.set last (uL nstr)
            in { nstr | u <- set_uL <| set_u0 nstr.u}
    in bc <| { str | t <- nt
             , u <- Array.indexedMap (\i x -> stepNode i x) str.x
             , u_old <- str.u}

preBoundary : String1D -> String1D
preBoundary str = str

postBoundary : String1D -> String1D
postBoundary str = str

string1d : (Float, Float) -> Float -> List Float -> List Float -> (Float -> Float -> Float) -> String1D
string1d (sx, ex) nx borders layers source =
    let x = Array.fromList <| Utils.linspace sx ex nx
        t = 0
        u = Array.map (source t) x
        q = Array.map (calcQ borders layers) x
    in { x = x
       , borders = borders
       , layers = layers
       , q = q
       , u = u
       , u_old = u
       , t = t
       , source = source
       , left = Fixed
       , right = Fixed}

calcQ : List Float -> List Float -> Float -> Float
calcQ borders layers x =
    let b_len = List.length borders
        lessers = List.filter (\a -> x < a) borders
        l_len = List.length lessers
        qa = Array.fromList layers
    in getOrZero (b_len-l_len) qa


whichLayer : String1D -> Float -> Int
whichLayer str px = List.length <| List.filter (\x -> x <= px) str.borders


addBorderAt : String1D -> Float -> String1D
addBorderAt str x =
    let layerID = whichLayer str x
        newLayer = getOrZero layerID <| Array.fromList str.layers
        (front, back) = Utils.splitList str.layers layerID
        newBorders = List.sort <| x :: str.borders
        newLayers = List.append front <| newLayer :: back
    in { str | borders <- newBorders
             , layers <- newLayers
             , q <- Array.map (calcQ newBorders newLayers) str.x}

removeBorderAt : String1D -> Int -> String1D
removeBorderAt str layerID =
    let newBorders = Utils.spliceList str.borders layerID 1
        newLayers = Utils.spliceList str.layers (layerID+1) 1
    in { str | borders <- newBorders
             , layers <- newLayers
             , q <- Array.map (calcQ newBorders newLayers) str.x}

updateLayerAt : String1D -> Int -> Float -> String1D
updateLayerAt str layerID newValue =
    let newLayers = Array.toList <| Array.set layerID newValue <| Array.fromList str.layers
    in { str | layers <- newLayers
             , q <- Array.map (calcQ str.borders newLayers) str.x}