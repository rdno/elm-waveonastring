import Graphics.Element (Element, flow, down, right)
import Graphics.Input as Input
import List
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
defaultStr = Model.string1d (0, 1) 300 boundaries qs f

-- Initiate Simulation

type State = Playing | Paused

type alias Simulation = { models : List Model.String1D,
                          state : State}

defaultSim = {models = [defaultStr], state = Paused}

-- Step

stepSim : Event -> Simulation -> Simulation
stepSim ev sim =
    let cur_model = List.head sim.models
        dt = 1/100
        safe_tail lst = if List.length lst > 1 then List.tail lst else lst
    in case ev of
      Tick t -> case sim.state of
                  Playing -> { sim | models <- Model.step cur_model dt :: sim.models}
                  Paused -> sim
      Button Pause -> { sim | state <- Paused }
      Button Play -> { sim | state <- Playing }
      Button Back -> { sim | state <- Paused
                     , models <- safe_tail sim.models}
      Button Forward -> { sim | state <- Paused
                        , models <-  Model.step cur_model dt :: sim.models}
      Button Default -> defaultSim

-- Events

type ButtonType = None | Play | Pause | Back | Forward | Default

buttontype : Signal.Channel ButtonType
buttontype = Signal.channel None


ticks = fps 100
type Event = Tick Time.Time | Button ButtonType
events = Signal.mergeMany [ Signal.map Tick ticks
                          , Signal.map Button (Signal.subscribe buttontype)]


-- Utils

type2label : ButtonType -> String
type2label bt =
    case bt of
      Play -> "Play"
      Pause -> "Pause"
      Back -> "Back"
      Forward -> "Forward"
      Default -> "Default"


-- Render UI

makeButton : ButtonType -> Element
makeButton bt = Input.button (Signal.send buttontype bt) (type2label bt)


playPauseButton sim =
    case sim.state of
      Playing -> makeButton Pause
      Paused  -> makeButton Play


buttons sim = flow right [ makeButton Back
                         , playPauseButton sim
                         , makeButton Forward
                         , makeButton Default]

renderSim sim = flow down [ Renderer.render (List.head sim.models)
                          , buttons sim]
main = renderSim <~ Signal.foldp stepSim defaultSim events