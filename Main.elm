import Signal ((<~))
import Signal
import Text (asText)
import Time (fps)

import Model
import Renderer
import Utils (exp)

f : Float -> Float-> Float
f t x = (sin (pi*x + 10*t))*(exp -((x-0.5)^2/0.001))*(exp -(t^2/0.1))

boundaries = [0.3, 0.7]
qs = [1, 5, 3]
str = Model.string1d (0, 1) 300 boundaries qs f


stepper dt str = Model.step str (1/100)
main = Renderer.render <~ Signal.foldp stepper str (fps 100)