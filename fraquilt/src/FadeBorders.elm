module FadeBorders exposing (Adjustments, Config, Direction(..), Flags, Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy
import Json.Decode
import List.Extra as List
import Random
import Task


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }


type alias Config =
    List Int


configToRbgString : Config -> String
configToRbgString list =
    case list of
        r :: g :: b :: _ ->
            "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

        _ ->
            "rgb(0,0,0)"


generateImage : Adjustments Config -> Int -> String -> String -> Config -> Html msg
generateImage adjustments level pathKey currentPosition config =
    if level == 0 then
        div
            [ class "box"
            , class currentPosition
            , id pathKey
            , Html.Attributes.style "background-color" (configToRbgString config)
            ]
            []

    else
        Keyed.node "div"
            [ class "box"
            , class currentPosition
            , Html.Attributes.style "border-color" (configToRbgString config)
            , Html.Attributes.style "border-width" "1px"
            , Html.Attributes.style "border-style" "solid"
            ]
            [ ( pathKey ++ "-tl"
              , generateImage adjustments
                    (level - 1)
                    (pathKey ++ "-tl")
                    "tl"
                    (adjustments.tl config)
              )
            , ( pathKey ++ "-tr"
              , generateImage adjustments
                    (level - 1)
                    (pathKey ++ "-tr")
                    "tr"
                    (adjustments.tr config)
              )
            , ( pathKey ++ "-bl"
              , generateImage adjustments
                    (level - 1)
                    (pathKey ++ "-bl")
                    "bl"
                    (adjustments.bl config)
              )
            , ( pathKey ++ "-br"
              , generateImage adjustments
                    (level - 1)
                    (pathKey ++ "-br")
                    "br"
                    (adjustments.br config)
              )
            ]


randomListShuffleFunction : Int -> Random.Generator (List Int -> List Int)
randomListShuffleFunction listLength =
    Random.list listLength (Random.int 0 (listLength - 1))
        |> Random.map
            (\listOfIndices ->
                \listInput ->
                    List.indexedMap
                        (\index item ->
                            let
                                swapInput =
                                    List.getAt index listOfIndices
                                        |> Maybe.withDefault index
                            in
                            List.getAt swapInput listInput
                                |> Maybe.withDefault item
                        )
                        listInput
            )


randomizeAdjustments : Int -> Random.Generator (Adjustments Config)
randomizeAdjustments listLength =
    let
        randomList =
            randomListShuffleFunction listLength
    in
    Random.map4 Adjustments
        randomList
        randomList
        randomList
        randomList


randomVariables : Int -> Random.Generator Config
randomVariables n =
    Random.list n (Random.int 0 255)


viewFrameworks : Model -> List ( String, Html Msg )
viewFrameworks model =
    List.range 0 maxLevel
        |> List.map
            (\level ->
                ( String.fromInt level
                , div
                    [ id ("level-" ++ String.fromInt level)
                    , Html.Attributes.style "opacity"
                        (if level <= model.level then
                            "1"

                         else
                            "0"
                        )
                    , Html.Attributes.style "transition" "opacity 0.5s linear"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "bottom" "0"
                    , Html.Attributes.style "right" "0"
                    , Html.Attributes.style "left" "0"
                    , if level == 0 then
                        onTransitionEnd Randomize

                      else
                        class ""
                    ]
                    [ Html.Lazy.lazy5 generateImage
                        model.adjustments
                        level
                        ("level-" ++ String.fromInt level)
                        "outer"
                        model.initialVariables
                    ]
                )
            )


cssStyles : String
cssStyles =
    """
div {
    box-sizing: border-box;
}

.box {
    height: 50%;
    width: 50%;
    position: absolute;
}

#container {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
}

.outer {
    position: relative;
    height: 100%;
    width: 100%;
}

.tl {
    top: 0;
    left: 0;
}

.tr {
    top: 0;
    right: 0;
}

.bl {
    bottom: 0;
    left: 0;
}

.br {
    bottom: 0;
    right: 0;
}
"""


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ Html.text cssStyles ]
        , Keyed.node "div"
            [ id "container"
            , if model.level == maxLevel then
                onClick AnimateLevel

              else
                class ""
            ]
            (viewFrameworks model)
        ]



--


type alias Model =
    { iteration : Int
    , adjustments : Adjustments Config
    , level : Int
    , initialVariables : Config
    , randomSeed : Random.Seed
    , numberOfVariables : Int -- Length of list
    , levelAnimationDirection : Direction
    , doNextAnimationFrame : List Msg
    }


type Direction
    = Up
    | Down
    | None


type alias Flags =
    { randomSeed : Int }


maxLevel : Int
maxLevel =
    7


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        numberOfVariables =
            6

        level =
            0

        seed =
            Random.initialSeed flags.randomSeed

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments
    in
    ( { iteration = 0
      , adjustments = adjustments
      , level = level
      , initialVariables = newInitialColor
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      , levelAnimationDirection = Up
      , doNextAnimationFrame = [ AnimateLevel ]
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | AnimateLevel
    | GotNextAnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            if model.level == -1 then
                let
                    ( randomizedAdjustments, seedAfterAdustments ) =
                        Random.step (randomizeAdjustments model.numberOfVariables) model.randomSeed

                    ( newInitialColor, newSeed ) =
                        Random.step (randomVariables model.numberOfVariables) seedAfterAdustments
                in
                ( { model
                    | adjustments = randomizedAdjustments
                    , initialVariables = newInitialColor
                    , randomSeed = newSeed
                    , doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        AnimateLevel ->
            let
                changeLevel dir m =
                    case dir of
                        Up ->
                            { m | level = m.level + 1 }

                        Down ->
                            { m | level = m.level - 1 }

                        None ->
                            m
            in
            if model.level == maxLevel then
                ( if model.levelAnimationDirection == None then
                    { model
                        | levelAnimationDirection = Down
                        , doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                    }
                        |> changeLevel Down

                  else
                    { model
                        | levelAnimationDirection = None
                    }
                , Cmd.none
                )

            else if model.level == -1 then
                case
                    model.levelAnimationDirection
                of
                    Down ->
                        ( { model
                            | levelAnimationDirection = Up
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( { model
                            | levelAnimationDirection = Up
                            , doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                          }
                            |> changeLevel Up
                        , Cmd.none
                        )

            else
                ( { model
                    | doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                  }
                    |> changeLevel model.levelAnimationDirection
                , Cmd.none
                )

        GotNextAnimationFrame ->
            case model.doNextAnimationFrame of
                first :: rest ->
                    ( { model | doNextAnimationFrame = rest }
                    , Task.perform identity (Task.succeed first)
                    )

                _ ->
                    ( model, Cmd.none )


onTransitionEnd : Msg -> Html.Attribute Msg
onTransitionEnd msg =
    Html.Events.on "transitionend" (Json.Decode.succeed msg)


subscriptions : Model -> Sub Msg
subscriptions { doNextAnimationFrame } =
    if List.isEmpty doNextAnimationFrame then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta
            (\_ ->
                GotNextAnimationFrame
            )
