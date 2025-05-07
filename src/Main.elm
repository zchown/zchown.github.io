module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Attributes as Attr
import Time
import Random exposing (Generator)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2, add, sub, scale, distance, length, normalize)
import Browser.Events

canvasWidth : Int
canvasWidth = 2000

canvasHeight : Int
canvasHeight = 2000

numBoids : Int
numBoids = 250

perceptionRadius : Float
perceptionRadius = 25

separationWeight : Float
separationWeight = 1.0

alignmentWeight : Float
alignmentWeight = 1.0

cohesionWeight : Float
cohesionWeight = 1.0

maxSpeed : Float
maxSpeed = 5.0
minSpeed : Float
minSpeed = 2.0
maxSteerForce : Float
maxSteerForce = 3.0

type alias Boid =
    { position : Vec2
    , velocity : Vec2
    , id : Int
    }

type alias Model =
    { boids : List Boid
    , nextId : Int
    , windowWidth : Int
    , windowHeight : Int
    }

type Msg
    = Tick Time.Posix
    | InitBoids (List Boid)
    | WindowResize Int Int

init : () -> ( Model, Cmd Msg )
init _ =
    ( { boids = []
      , nextId = 0
      , windowWidth = 2000
      , windowHeight = 2000
      }
    , generateRandomBoids numBoids 0
    )

generateRandomBoids : Int -> Int -> Cmd Msg
generateRandomBoids count startId =
    Random.generate InitBoids (Random.list count (randomBoid startId))

randomBoid : Int -> Generator Boid
randomBoid startId =
    Random.map3
        (\pos vel id -> 
            { position = pos
            , velocity = vel
            , id = id
            }
        )
        randomPosition
        randomVelocity
        (Random.int startId (startId + numBoids * 2))

randomPosition : Generator Vec2
randomPosition =
    Random.map2 
        vec2
        (Random.float 0 (toFloat 2000))
        (Random.float 0 (toFloat 2000))

randomVelocity : Generator Vec2
randomVelocity =
    Random.map2
        (\angle speed ->
            let
                vx = speed * cos angle
                vy = speed * sin angle
            in
            vec2 vx vy
        )
        (Random.float 0 (2 * pi))
        (Random.float minSpeed maxSpeed)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | boids = List.map (updateBoid model) model.boids }
            , Cmd.none
            )
        InitBoids newBoids ->
            ( { model | boids = newBoids, nextId = model.nextId + numBoids }
            , Cmd.none
            )
        WindowResize width height ->
            ( { model | windowWidth = width, windowHeight = height }
            , Cmd.none
            )

updateBoid : Model -> Boid -> Boid
updateBoid model boid =
    let
        neighbors = 
            List.filter 
                (\other -> other.id /= boid.id && 
                           distance other.position boid.position < perceptionRadius) 
                model.boids
        separation = calculateSeparation neighbors boid
        alignment = calculateAlignment neighbors
        cohesion = calculateCohesion neighbors boid
        steerForce = 
            add 
                (V2.scale separationWeight separation) 
                (add 
                    (V2.scale alignmentWeight alignment) 
                    (V2.scale cohesionWeight cohesion)
                )
        limitedSteer = limitForce steerForce maxSteerForce
        newVelocity = 
            boid.velocity
                |> add limitedSteer
                |> limitVelocity
        newPosition = 
            boid.position
                |> add newVelocity
                |> wrapEdges model
    in
    { boid | position = newPosition, velocity = newVelocity }

calculateSeparation : List Boid -> Boid -> Vec2
calculateSeparation neighbors boid =
    case neighbors of
        [] -> 
            vec2 0 0
        _ ->
            let
                steer =
                    List.foldl
                        (\other acc ->
                            let
                                diff = sub boid.position other.position
                                dist = length diff
                                weighted = 
                                    if dist > 0 then
                                        V2.scale (1 / (dist * dist)) diff
                                    else
                                        vec2 0 0
                            in
                            add acc weighted
                        )
                        (vec2 0 0)
                        neighbors
            in
            if length steer > 0 then
                normalize steer
            else
                vec2 0 0

calculateAlignment : List Boid -> Vec2
calculateAlignment neighbors =
    case neighbors of
        [] ->
            vec2 0 0

        _ ->
            let
                avgVelocity =
                    List.foldl (\b acc -> add acc b.velocity) (vec2 0 0) neighbors
                        |> V2.scale (1 / toFloat (List.length neighbors))
            in
            if length avgVelocity > 0 then
                normalize avgVelocity
            else
                vec2 0 0

calculateCohesion : List Boid -> Boid -> Vec2
calculateCohesion neighbors boid =
    case neighbors of
        [] -> 
            vec2 0 0
        _ ->
            let
                centerOfMass =
                    List.foldl (\b acc -> add acc b.position) (vec2 0 0) neighbors
                        |> V2.scale (1 / toFloat (List.length neighbors))
                desired = sub centerOfMass boid.position
            in
            if length desired > 0 then
                normalize desired
            else
                vec2 0 0

limitForce : Vec2 -> Float -> Vec2
limitForce force maxForce =
    let
        mag = length force
    in
    if mag > maxForce && mag > 0 then
        V2.scale (maxForce / mag) force
    else
        force

limitVelocity : Vec2 -> Vec2
limitVelocity vel =
    let
        mag = length vel
    in
    if mag > maxSpeed && mag > 0 then
        V2.scale (maxSpeed / mag) vel
    else if mag < minSpeed && mag > 0 then
        V2.scale (minSpeed / mag) vel
    else if mag == 0 then
        vec2 (minSpeed / 2) (minSpeed / 2)
    else
        vel

wrapEdges : Model -> Vec2 -> Vec2
wrapEdges model pos =
    let
        x = getX pos
        y = getY pos
        w = toFloat model.windowWidth
        h = toFloat model.windowHeight
        newX = 
            if x < 0 then
                x + w
            else if x > w then
                x - w
            else
                x
        newY =
            if y < 0 then
                y + h
            else if y > h then
                y - h
            else
                y
    in
    vec2 newX newY

view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "overflow" "hidden"
        ]
        [ svg
            [ Attr.width "100%"
            , Attr.height "100%"
            , Attr.viewBox ("0 0 " ++ String.fromInt model.windowWidth ++ " " ++ String.fromInt model.windowHeight)
            , Attr.style "display: block"
            , Attr.preserveAspectRatio "none"
            ]
            (List.map viewBoid model.boids)
        ]

viewBoid : Boid -> Svg Msg
viewBoid boid =
    let
        angle = 
            if length boid.velocity > 0 then
                atan2 (getY boid.velocity) (getX boid.velocity) * 180 / pi
            else
                0
        transformAttr =
            "translate(" ++ String.fromFloat (getX boid.position)
                ++ "," ++ String.fromFloat (getY boid.position)
                ++ ") rotate(" ++ String.fromFloat angle ++ ")"
    in
    polygon
        [ points "-5,0 5,0 0,10"
        , fill "#3366cc"
        , stroke "#000000"
        , strokeWidth "1"
        , transform transformAttr
        ]
        []

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 60) Tick
        , Browser.Events.onResize WindowResize
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
