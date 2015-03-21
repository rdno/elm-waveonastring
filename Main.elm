import Graphics.Element (Element, flow, down, right)
import Graphics.Input as Input
import Signal ((<~))
import Signal
import Text (asText)
import Time (fps)
import Time

import Model
import Renderer
import Utils (exp)

-- Initiate Model

f : Float -> Float-> Float
f t x = (sin (pi*x + 10*t))*(exp -((x-0.5)^2/0.001))*(exp -(t^2/0.1))

boundaries = [0.3, 0.7]
qs = [1, 5, 3]
str = Model.string1d (0, 1) 300 boundaries qs f

-- Initiate Simulation

type State = Playing | Paused

type alias Simulation = { model : Model.String1D,
                          state : State}

sim = {model = str, state = Playing}

-- Step

stepSim : Event -> Simulation -> Simulation
stepSim ev sim =
    case ev of
      Tick t -> case sim.state of
                  Playing -> { sim | model <- Model.step sim.model (1/100)}
                  Paused -> sim
      Button Pause -> {sim | state <- Paused }
      Button Play -> {sim | state <- Playing }

-- Events

type ButtonType = None | Play | Pause

buttontype : Signal.Channel ButtonType
buttontype = Signal.channel None

ticks = fps 100
type Event = Tick Time.Time | Button ButtonType
events = Signal.mergeMany [ Signal.map Tick ticks
                          , Signal.map Button (Signal.subscribe buttontype)]


-- Render UI

playPauseButton sim =
    case sim.state of
      Playing -> Input.button (Signal.send buttontype Pause) "Pause"
      Paused  -> Input.button (Signal.send buttontype Play) "Play"


renderSim sim = flow down [ Renderer.render sim.model
                          , playPauseButton sim]
main = renderSim <~ Signal.foldp stepSim sim events