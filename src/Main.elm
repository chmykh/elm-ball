module Main exposing (..)

import Browser
import Browser.Events
import Browser.Dom
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Time
import Task
import Json.Decode
import Json.Encode
import Html.Events.Extra.Touch as Touch

-- the whole modifiable program state
type alias Model =
  { text : String
  , y : Float -- the ball position in visual (svg viewbox) coordinates
  , v : Float
  , h : Float -- the ground line position in visual coordinates
  , lastDragPoint : Maybe Float -- y of last drag event, in whatever coordinates as long as scale 100% to viewbox
  , width : Float -- visual width
  , height : Float -- visual height
  }

-- all possible messages to update the program state
type Msg =
  Tick Float
  | Drag Float
  | StartDrag Float
  | EndDrag
  | Resize Int Int

const =
  { radius = 50
  , spring = 10
  , gravity = 10
  , drag = 0 -- if set to nonzero the ball will eventually stop (unrealistically). 0.1 is a reasonable value
  , rate = 5 -- speed up simulation, for visual purposes
  }

-- initial state and message to be sent at startup
init () =
  let
    initialState = Model "" const.radius 0 0 Nothing 0 0
    resizeToViewport vp = Resize (round vp.viewport.width) (round vp.viewport.height)
  in
    (initialState, Task.perform resizeToViewport Browser.Dom.getViewport)

-- perform simulation for 'rt' microsecond of real time
recalc rt =
  let
    step dt model = -- perform single step of 'dt' microsecond of simulated time
      let
        my = model.y + 0.5 * model.v * dt
        sy = clamp 0 const.radius (my + const.radius - model.h) -- compression delta
        fh = -const.spring * sy -- spring force
        fd = -model.v * const.drag -- viscous drag force
        a = const.gravity + fh + fd
        v = model.v + a * dt
        mv = (model.v + v) * 0.5
        y = model.y + mv * dt
      in
        { model | v = v, y = y }
    steps dt n model = -- perform n steps of 'dt' microsecond of simulated time each
       List.repeat n 0 |> List.foldl (\_ -> step dt) model
  in
    steps 0.001 (const.rate * round rt)

-- format a text line with model state and energy data
annotate model = 
  let
    h = model.h - model.y
    k = model.v * model.v * 0.5
    p = const.gravity * (model.h - model.y)
    sy = clamp 0 const.radius (model.y + const.radius - model.h)
    d = 0.5 * const.spring * sy * sy
    e = k + p + d
    print = round >> String.fromInt >> String.padLeft 5 ' '
    text =
      "h=" ++ print h ++
      " v=" ++ print model.v ++
      " K=" ++ print k ++
      " P=" ++ print p ++
      " D=" ++ print d ++
      " E=" ++ print e 
  in { model | text = text }

-- update model according to message (recalc, drag, or resize)
update msg model =
  let
    save point m = { m | lastDragPoint = Just point }
    drop m = { m | lastDragPoint = Nothing }
    minH = const.radius * 1.5
    maxH = model.height - const.radius * 0.5
    shift m point lastPoint = { m | h = clamp minH maxH (m.h + point - lastPoint) }
    drag point m = m.lastDragPoint |> Maybe.map (shift m point >> save point) |> Maybe.withDefault m
  in
  (case msg of     
    Tick rt -> model |> recalc rt |> annotate
    Drag point -> drag point model
    StartDrag point -> save point model
    EndDrag -> drop model
    Resize w h ->
      let
        fw = toFloat w * 0.9
        fh = toFloat h * 0.5
      in
        { model | width = fw, height = fh, h = fh - const.radius }
  , Cmd.none)

-- events from the non-elm world
subscriptions model = 
  Sub.batch
    [ Browser.Events.onAnimationFrameDelta Tick
    , Browser.Events.onResize Resize ]

-- the html element
view model =
  let 
    yRadius = if model.h - model.y > const.radius then const.radius else (model.h - model.y + const.radius) / 2.0
    yPos = if model.h - model.y > const.radius then model.y else model.h - (model.h - model.y + const.radius) / 2.0
    withMouseY g = Json.Decode.field "clientY" Json.Decode.float |> Json.Decode.map g
    withTouchY e = List.head e.changedTouches |> Maybe.map (.clientPos >> Tuple.second) |> Maybe.withDefault 0
  in
    div [] [
      svg
        [ viewBox ("0 0 " ++ String.fromFloat model.width ++ " " ++ String.fromFloat model.height)
        , width (String.fromFloat model.width)
        , height (String.fromFloat model.height)
        , Svg.Attributes.style "background-color:#efefef"
        , Svg.Events.on "mousedown" (withMouseY StartDrag)
        , Svg.Events.on "mousemove" (withMouseY Drag)
        , Svg.Events.onMouseUp EndDrag
        , Svg.Events.onMouseOut EndDrag
        , Touch.onStart (withTouchY >> StartDrag)
        , Touch.onMove (withTouchY >> Drag)
        , Touch.onEnd (always EndDrag)
        ]
        [ ellipse
          [ cx "200"
          , cy (String.fromFloat yPos)
          , rx (String.fromFloat const.radius)
          , ry (String.fromFloat yRadius)
          , fill "#1293A8"
          ]
          [],
          line
          [ x1 "0"
          , y1 (String.fromFloat (model.h + 2))
          , x2 (String.fromFloat model.width)
          , y2 (String.fromFloat (model.h + 2))
          , stroke "black"
          , strokeWidth "4"]
          []
        ]
      , pre [] [Html.text model.text]
      ]

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

