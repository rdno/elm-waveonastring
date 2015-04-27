import Graphics.Element (Element, flow, down, right, spacer)
import Graphics.Input as Input
import List
import Mouse
import Signal ((<~))
import Signal
import Text (asText)
import Text
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
qs = [0.5, 0.25, 0.15]
defaultStr = Model.string1d (0, 1) 500 boundaries qs f

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
    let dt = 1/500
        safe_tail lst = if List.length lst > 1 then List.tail lst else lst
        -- TODO: do not store environment changes instead modify the last one
        modelSizeLimit = floor (1/dt*10)  -- store 10 sn of data
        add_model model =
            if List.length sim.models < modelSizeLimit
            then model :: sim.models
            else model :: (List.take (modelSizeLimit - 1) sim.models)
        cur_model = curModel sim
        whichLayer coord = Model.whichLayer cur_model <| Renderer.getX coord
        vToQ v = v^2
        updateLayer coord = Model.updateLayerAt cur_model (whichLayer coord) (vToQ (Renderer.getV coord))
    in case ev of
      Tick t -> case sim.state of
                  Playing -> { sim | models <- add_model <| Model.step cur_model dt}
                  Paused -> sim
      Button Pause -> { sim | state <- Paused }
      Button Play -> { sim | state <- Playing }
      Button Back -> { sim | state <- Paused
                     , models <- safe_tail sim.models}
      Button Forward -> { sim | state <- Paused
                        , models <- add_model <| Model.step cur_model dt}
      Button Default -> defaultSim
      MouseMove pos ->  let updatedSim = {sim | lastMousePos <- pos}
                        in case Renderer.canvasMousePosition pos of
                             Just coord ->
                              case sim.editMode of
                                MoveQ ->  { updatedSim | models <- add_model <| updateLayer coord }
                                otherwise -> updatedSim
                             Nothing -> updatedSim
      MouseDown False -> { sim | editMode <- NoEdit}
      MouseDown True -> case Renderer.canvasMousePosition sim.lastMousePos of
                          Just coord ->
                              case sim.editMode of
                                NoEdit -> { sim | models  <- add_model <| updateLayer coord
                                          ,       editMode <- MoveQ}
                                AddBorder -> { sim | models <- add_model <| Model.addBorderAt cur_model <| Renderer.getX coord
                                             ,       editMode <- NoEdit}
                                RemoveBorder -> let px = Renderer.getX coord
                                                    n = List.filter (\(i, x) -> x) <| List.indexedMap (\i x -> (i, Utils.near x px 0.01)) cur_model.borders
                                                    layerID = if List.isEmpty n then List.length cur_model.layers else fst <| List.head n
                                                in { sim | models <- add_model <| Model.removeBorderAt cur_model layerID
                                                   ,       editMode <- NoEdit}
                          Nothing -> { sim | editMode <- NoEdit }
      Button AddBorderBtn -> { sim | editMode <- AddBorder }
      Button RemoveBorderBtn -> { sim | editMode <- RemoveBorder }
      BoundaryChanged (Just (b, Left)) -> { sim | models <- add_model <| {cur_model | left <- b}}
      BoundaryChanged (Just (b, Right)) -> { sim | models <- add_model <| {cur_model | right <- b}}


-- Events


type ButtonType = None | Play | Pause | Back | Forward
                | AddBorderBtn | RemoveBorderBtn | Default

buttontype : Signal.Channel ButtonType
buttontype = Signal.channel None

type Place = Left | Right

boundary : Signal.Channel (Maybe (Model.Boundary, Place))
boundary = Signal.channel Nothing

ticks = fps 100
type Event = Tick Time.Time | Button ButtonType
           | MouseMove (Int, Int) | MouseDown Bool | BoundaryChanged (Maybe (Model.Boundary, Place))
events = Signal.mergeMany [ Signal.map Tick ticks
                          , Signal.map Button (Signal.subscribe buttontype)
                          , Signal.map MouseMove Mouse.position
                          , Signal.map MouseDown Mouse.isDown
                          , Signal.map BoundaryChanged (Signal.subscribe boundary)]


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

makeDropdown place = Input.dropDown (Signal.send boundary) [
                      ("Fixed", Just (Model.Fixed, place))
                     ,("Loose",  Just (Model.Loose, place))]

label value = Text.leftAligned <| Text.fromString value

boundaryDropdowns = flow right [ label "Left: "
                               , spacer 10 10
                               , makeDropdown Left
                               , spacer 10 10
                               , label "Right: "
                               , spacer 10 10
                               , makeDropdown Right]


renderSim sim = flow down [(flow down [Renderer.render (curModel sim), buttons sim])
                          , spacer 10 10
                          , borderButtons
                          , spacer 10 10
                          , boundaryDropdowns]

main = renderSim <~ Signal.foldp stepSim defaultSim events