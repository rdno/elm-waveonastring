module Utils where

import List

e = 2.718281828459
exp x = e^x

ones n = List.map (\_ -> 1) [1..n]
zeros n = List.map (\_ -> 0) [1..n]

linspace : Float -> Float -> Float -> List Float
linspace start stop n =
    let step = (stop-start) / (n-1)
        value x = x*step + start
    in List.map value [0..n-1]
