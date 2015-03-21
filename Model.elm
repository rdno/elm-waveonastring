module Model where

import Array
import List
import Maybe

import Utils


type Boundary = Fixed | Loose | None

type alias String1D = { x : Array.Array Float
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
        stepNode i _ =
            if | i == 0    ->  getU str i
               | i == last ->  getU str i
               | otherwise ->  finiteDifference str i dt
    in { str | t <- nt
       , u <- Array.indexedMap (\i x -> stepNode i x) str.x
       , u_old <- str.u}

preBoundary : String1D -> String1D
preBoundary str = str

postBoundary : String1D -> String1D
postBoundary str = str

string1d : (Float, Float) -> Float -> List Float -> List Float -> (Float -> Float -> Float) -> String1D
string1d (sx, ex) nx boundaries qs source =
    let x = Array.fromList <| Utils.linspace sx ex nx
        t = 0
        u = Array.map (source t) x
        q = Array.map (calcQ boundaries qs) x
    in { x = x
       , q = q
       , u = u
       , u_old = u
       , t = t
       , source = source
       , left = None
       , right = None}

calcQ : List Float -> List Float -> Float -> Float
calcQ boundaries qs x =
    let b_len = List.length boundaries
        lessers = List.filter (\a -> x < a) boundaries
        l_len = List.length lessers
        qa = Array.fromList qs
    in getOrZero (b_len-l_len) qa