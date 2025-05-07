module Main exposing (..)

import Browser
import Json.Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Time
import Random exposing (Generator)
import Math.Vector2 as V2 exposing (Vec2, getX, getY, vec2, add, sub, scale, distance, length, normalize)
import Browser.Events

-- CONSTANTS

canvasWidth : Int
canvasWidth = 2000

canvasHeight : Int
canvasHeight = 2000

numBoids : Int
numBoids = 200

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

-- MODEL

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
    , scrollPosition : Float
    }

type Msg
    = Tick Time.Posix
    | InitBoids (List Boid)
    | WindowResize Int Int
    | ScrollUpdate Float

init : () -> ( Model, Cmd Msg )
init _ =
    ( { boids = []
      , nextId = 0
      , windowWidth = canvasWidth
      , windowHeight = canvasHeight
      , scrollPosition = 0
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
        (Random.float 0 (toFloat canvasWidth))
        (Random.float 0 (toFloat canvasHeight))

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

-- UPDATE

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

        ScrollUpdate pos ->
            ( { model | scrollPosition = pos }
            , Cmd.none
            )

updateBoid : Model -> Boid -> Boid
updateBoid model boid =
    let
        neighbors =
            List.filter
                (\other ->
                    other.id /= boid.id
                        && distance other.position boid.position < perceptionRadius
                )
                model.boids

        separation = calculateSeparation neighbors boid
        alignment  = calculateAlignment neighbors
        cohesion   = calculateCohesion neighbors boid

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
                                diff     = V2.sub boid.position other.position
                                dist     = length diff
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
                avgVel =
                    List.foldl (\b acc -> add acc b.velocity) (vec2 0 0) neighbors
                        |> V2.scale (1 / toFloat (List.length neighbors))
            in
            if length avgVel > 0 then
                normalize avgVel
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

                desired =
                    V2.sub centerOfMass boid.position
            in
            if length desired > 0 then
                normalize desired
            else
                vec2 0 0

limitForce : Vec2 -> Float -> Vec2
limitForce force maxF =
    let
        mag = length force
    in
    if mag > maxF && mag > 0 then
        V2.scale (maxF / mag) force
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
            if x < 0 then x + w else if x > w then x - w else x

        newY =
            if y < 0 then y + h else if y > h then y - h else y
    in
    vec2 newX newY

-- VIEW

view : Model -> Html Msg
view model =
    div
        [ style "position" "relative"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "margin" "0"
        , style "padding" "0"
        , style "overflow" "hidden"
        ]
        [ viewNavigation
        , viewBoidsLayer model
        , viewContentOverlay model
        ]

viewNavigation : Html Msg
viewNavigation =
    nav
        [ style "position"         "fixed"
        , style "top"              "0"
        , style "left"             "0"
        , style "width"            "100%"
        , style "z-index"          "3"
        , style "background-color" "#1D3B53"
        , style "padding"          "15px 0"
        , style "box-shadow"       "0 2px 5px rgba(0,0,0,0.2)"
        ]
        [ ul
            [ style "list-style-type" "none"
            , style "margin"          "0"
            , style "padding"         "0"
            , style "display"         "flex"
            , style "justify-content" "center"
            ]
            [ viewNavItem "home" "Home"
            , viewNavItem "about" "About"
            , viewNavItem "projects" "Projects"
            , viewNavItem "contact" "Contact"
            ]
        ]

viewNavItem : String -> String -> Html Msg
viewNavItem targetId label =
    li
        [ style "margin" "0 15px" ]
        [ a
            [ href ("#" ++ targetId)
            , style "text-decoration" "none"
            , style "color"           "#D6DEEB"
            , style "font-weight"     "bold"
            , style "font-size"       "1.1rem"
            ]
            [ text label ]
        ]

viewBoidsLayer : Model -> Html Msg
viewBoidsLayer model =
    div
        [ style "position" "fixed"
        , style "top"      "0"
        , style "left"     "0"
        , style "width"    "100%"
        , style "height"   "100%"
        , style "z-index"  "1"
        , style "background-color" "#011627"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            , SvgAttr.viewBox ("0 0 " ++ String.fromInt model.windowWidth ++ " " ++ String.fromInt model.windowHeight)
            , SvgAttr.style "display: block"
            , SvgAttr.preserveAspectRatio "none"
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
            "translate("
                ++ String.fromFloat (getX boid.position)
                ++ ","
                ++ String.fromFloat (getY boid.position)
                ++ ") rotate("
                ++ String.fromFloat angle
                ++ ")"
    in
    Svg.polygon
        [ SvgAttr.points "-5,0 5,0 0,10"
        , SvgAttr.fill "#D6DEEB"
        , SvgAttr.stroke "#D6DEEB"
        , SvgAttr.strokeWidth "1"
        , SvgAttr.transform transformAttr
        ]
        []

viewContentOverlay : Model -> Html Msg
viewContentOverlay model =
    div
        [ style "position"         "absolute"
        , style "top"              "0"
        , style "left"             "0"
        , style "width"            "100%"
        , style "height"           "100%"
        , style "z-index"          "2"
        , style "overflow-y"       "auto"
        , style "scroll-behavior"  "smooth"
        ]
        [ div
            [ style "width"            "80%"
            , style "max-width"        "1200px"
            , style "margin"           "0 auto"
            , style "padding"          "100px 20px 40px"
            , style "min-height"       "100vh"
            , style "background-color" "rgba(29, 59, 83, 0.6)"
            , style "backdrop-filter"  "blur(2px)"
            , style "box-shadow"       "0 0 20px rgba(255, 255, 255, 0.7)"
            ]
            [ viewSection "home" "Zander Chown" "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam in dui mauris. Vivamus hendrerit arcu sed erat molestie vehicula. Sed auctor neque eu tellus rhoncus ut eleifend nibh porttitor."
            , viewSection "about" "About" "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam in dui mauris. Vivamus hendrerit arcu sed erat molestie vehicula. Sed auctor neque eu tellus rhoncus ut eleifend nibh porttitor."
            , viewSection "projects" "Projects" "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium."
            , viewSection "contact" "Contact" "Interested in collaborating or have questions? Feel free to reach out. Email: example@domain.com"
            ]
        ]

viewSection : String -> String -> String -> Html Msg
viewSection sectionId title content =
    div
        [ id sectionId
        , style "padding"          "20px"
        , style "margin-bottom"    "40px"
        , style "background-color" "#1D3B53"
        , style "border-radius"    "8px"
        ]
        [ h2
            [ style "color"      "#82AAFF"
            , style "font-size"  (if sectionId == "home" then "2.5rem" else "1.8rem")
            , style "margin-bottom" "1rem"
            ]
            [ text title ]
        , p
            [ style "line-height" "1.6"
            , style "color"       "#D6DEEB"
            ]
            [ text content ]
        ]

-- DECODE SCROLL

scrollPositionDecoder : Json.Decode.Decoder Float
scrollPositionDecoder =
    Json.Decode.field "target" (Json.Decode.field "scrollTop" Json.Decode.float)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 60) Tick
        , Browser.Events.onResize WindowResize
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

