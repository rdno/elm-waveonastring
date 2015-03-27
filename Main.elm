import Graphics.Element (Element, flow, down, right)
import Graphics.Input as Input
import List
import Mouse
import Signal ((<~))
import Signal
import Text (asText)
import Time (fps)
import Time

import Model
import Renderer
import Utils (exp)
import Utils

-- Initiate Model

f : Float -> Float-> Float
f t x = (sin (pi*x + 10*t))*(exp -((x-0.5)^2/0.001))*(exp -(t^2/0.1))

boundaries = [0.3, 0.7]
qs = [1, 5, 3]
defaultStr = Model.string1d (0, 1) 300 boundaries qs f

-- Initiate Simulation

type State = Playing | Paused
type EditMode = AddBorder | RemoveBorder | MoveQ | NoEdit

type alias Simulation = { models : List Model.String1D
                        , state : State
                        , editMode : EditMode
                        , lastMousePos : (Int, Int)}

defaultSim = { models = [defaultStr], state = Paused
             , editMode = NoEdit, lastMousePos = (0, 0)}

curModel sim = List.head sim.models

-- Step

stepSim : Event -> Simulation -> Simulation
stepSim ev sim =
    let dt = 1/100
        safe_tail lst = if List.length lst > 1 then List.tail lst else lst
        add_model model = model :: sim.models
        whichLayer pos = Model.whichLayer (curModel sim) <| Renderer.asX pos
        updateLayer pos = Model.updateLayerAt (curModel sim) (whichLayer pos) (Renderer.layerRelativeY pos)
    in case ev of
      Tick t -> case sim.state of
                  Playing -> { sim | models <- add_model <| Model.step (curModel sim) dt}
                  Paused -> sim
      Button Pause -> { sim | state <- Paused }
      Button Play -> { sim | state <- Playing }
      Button Back -> { sim | state <- Paused
                     , models <- safe_tail sim.models}
      Button Forward -> { sim | state <- Paused
                        , models <- add_model <| Model.step (curModel sim) dt}
      Button Default -> defaultSim
      MouseMove pos ->  if Renderer.inCollage pos then
                            case sim.editMode of
                              MoveQ -> { sim | models <- add_model <|  updateLayer pos
                                       , lastMousePos <- pos}
                              otherwise -> { sim | lastMousePos <- pos}
                        else {sim | lastMousePos <- pos}
      MouseDown t -> if t && Renderer.inCollage sim.lastMousePos then
                         case sim.editMode of
                           NoEdit -> { sim | models <- add_model <|  updateLayer sim.lastMousePos
                                             , editMode <- MoveQ}
                           AddBorder -> let newBorder = Renderer.asX sim.lastMousePos
                                        in {sim | models <- add_model <|  Model.addBorderAt (curModel sim) newBorder
                                           , editMode <- NoEdit}
                           RemoveBorder -> let px = Renderer.asX sim.lastMousePos
                                               m = curModel sim
                                               n = List.filter (\(i, x) -> x) <| List.indexedMap (\i x -> (i, Utils.near x px 0.01)) m.borders
                                               layerID = if List.isEmpty n then List.length m.layers else fst <| List.head n
                                           in {sim | models <- add_model <| Model.removeBorderAt m layerID
                                                    , editMode <- NoEdit}
                           otherwise -> sim
                     else { sim | editMode <- NoEdit}
      Button AddBorderBtn -> { sim | editMode <- AddBorder }
      Button RemoveBorderBtn -> { sim | editMode <- RemoveBorder }


-- Events


type ButtonType = None | Play | Pause | Back | Forward
                | AddBorderBtn | RemoveBorderBtn | Default

buttontype : Signal.Channel ButtonType
buttontype = Signal.channel None

ticks = fps 100
type Event = Tick Time.Time | Button ButtonType
           | MouseMove (Int, Int) | MouseDown Bool
events = Signal.mergeMany [ Signal.map Tick ticks
                          , Signal.map Button (Signal.subscribe buttontype)
                          , Signal.map MouseMove Mouse.position
                          , Signal.map MouseDown Mouse.isDown]


-- Utils

type2label : ButtonType -> String
type2label bt =
    case bt of
      Play -> "Play"
      Pause -> "Pause"
      Back -> "Back"
      Forward -> "Forward"
      AddBorderBtn -> "Add Border"
      RemoveBorderBtn -> "Remove Border"
      Default -> "Default"

roundi : Float -> Float -> Float
roundi i a = (toFloat (round (10^i*a)))/10^i

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
                         , makeButton Default
                         , asText <| roundi 2 ((curModel sim).t)]

borderButtons = flow right [makeButton AddBorderBtn, makeButton RemoveBorderBtn]

renderSim sim = flow down [(flow down [Renderer.render (curModel sim), buttons sim]), borderButtons]
main = renderSim <~ Signal.foldp stepSim defaultSim events