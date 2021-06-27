module Main exposing (..)

import Browser
import Browser.Events
import Browser.Dom
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (on)
import Time
import Task
import Json.Decode
import Json.Encode
import Html.Events.Extra.Touch as Touch

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { text : String
  , y : Float
  , v : Float
  , h : Float
  , lastDragPoint : Maybe Float
  , width : Float
  , height : Float
  }

type Msg = Tick Float | Drag Float | StartDrag Float | EndDrag | Resize Int Int

const =
  { radius = 50
  , spring = 1
  , gravity = 1
  , drag = 0 -- 0.1 is reasonable, 0.01 noticeable
  }

vp2resize : Browser.Dom.Viewport -> Msg
vp2resize vp = Resize (round vp.viewport.width) (round vp.viewport.height)

init : () -> (Model, Cmd Msg)
init _ = ( Model "" const.radius 0 0 Nothing 0 0, Task.perform vp2resize Browser.Dom.getViewport )

recalc rt =
  let
    step dt model =
      let
        my = model.y + 0.5 * model.v * dt
        sy = clamp 0 const.radius (my + const.radius - model.h)
        fh = -const.spring * sy
        fd = -model.v * const.drag
        a = const.gravity + fh + fd
        v = model.v + a * dt
        mv = (model.v + v) * 0.5
        y = model.y + mv * dt
      in
        { model | v = v, y = y }
    steps dt n model =
       -- if n == 0 then model else step dt model |> steps dt (n - 1)
       List.repeat n 0 |> List.foldl (\_ m -> step dt m) model
    accuracy = 100
  in
    steps (0.01 / accuracy) (accuracy * round rt)

annotate model = 
  let
    h = model.h - model.y
    k = model.v * model.v * 0.5
    p = const.gravity * (model.h - model.y)
    sy = clamp 0 const.radius (model.y + const.radius - model.h)
    d = 0.5 * const.spring * sy * sy
    e = k + p + d
    print = round >> String.fromInt >> String.pad 5 ' '
    text =
      "h=" ++ print h ++
      " v=" ++ print model.v ++
      " K=" ++ print k ++
      " P=" ++ print p ++
      " D=" ++ print d ++
      " E=" ++ print e 
  in { model | text = text }
     
update msg model =
  let
    save point m = { m | lastDragPoint = Just point }
    drop m = { m | lastDragPoint = Nothing }
    shift m point lastPoint =
      { m | h = clamp (const.radius * 1.5) model.height (m.h + point - lastPoint) }
    newH point lastPoint m = m.h + lastPoint - point
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


subscriptions model = 
  Sub.batch
    [ Browser.Events.onAnimationFrameDelta Tick
    , Browser.Events.onResize Resize ]

view model =
  let 
    yRadius =
      if model.h - model.y > const.radius then const.radius else (model.h - model.y + const.radius) / 2.0
    yPos =
      if model.h - model.y > const.radius then model.y else model.h - (model.h - model.y + const.radius) / 2.0
    withMouseY g = Json.Decode.field "clientY" Json.Decode.float |> Json.Decode.map g
    withTouchY e = List.head e.changedTouches |> Maybe.map .clientPos |> Maybe.map Tuple.second |> Maybe.withDefault 0
  in
    Html.div [] [
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
      , Html.pre [] [text model.text]
      ]

