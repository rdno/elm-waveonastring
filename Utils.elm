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

-- Is distance between numbers  within `tolerance`
near : Float -> Float -> Float -> Bool
near a b tolerance = abs(a-b) < tolerance

-- Split list into two at index
splitList : List a -> Int -> (List a, List a)
splitList lst i = (List.take i lst, List.drop i lst)

-- Splice list at `i` and remove `count` items
spliceList : List a -> Int -> Int -> List a
spliceList lst i count =
    let (front, back) = splitList lst i
    in List.append front <| List.drop count back
