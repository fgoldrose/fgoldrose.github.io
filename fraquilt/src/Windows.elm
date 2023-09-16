module Windows exposing (Adjustments, Config, ConfigParams, Memoized, Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import Dict
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import List.Extra as List
import Random
import Random.List
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


configToBorderStyle : Config -> List (Html.Attribute Msg)
configToBorderStyle list =
    case list of
        l :: r :: t :: b :: _ ->
            [ Html.Attributes.style "border-left-width" (String.fromInt l ++ "px")
            , Html.Attributes.style "border-right-width" (String.fromInt r ++ "px")
            , Html.Attributes.style "border-top-width" (String.fromInt t ++ "px")
            , Html.Attributes.style "border-bottom-width" (String.fromInt b ++ "px")
            ]

        _ ->
            []


type alias Memoized =
    Dict.Dict Config
        { adjust :
            { tl : Config
            , tr : Config
            , bl : Config
            , br : Config
            }

        -- , levelImages : Dict.Dict Int (List (Html Msg))
        }


type alias ConfigParams =
    { config : Config
    , adjustments : Adjustments Config
    , memoized : Memoized
    }


generateImage : ConfigParams -> ConfigParams -> Int -> String -> String -> ( Html Msg, ConfigParams, ConfigParams )
generateImage colorConfigParams borderConfigParams level pathKey currentPosition =
    if level == 0 then
        ( div
            [ class "box"
            , class currentPosition
            , id pathKey
            , Html.Attributes.style "background-color" (configToRbgString colorConfigParams.config)
            ]
            []
        , colorConfigParams
        , borderConfigParams
        )

    else
        let
            wrapImages subImages =
                Keyed.node "div"
                    ([ class "box"
                     , class currentPosition
                     , Html.Attributes.style "border-style" "solid"
                     , Html.Attributes.style "border-color" (configToRbgString colorConfigParams.config)
                     , Html.Attributes.style "background-color" (configToRbgString colorConfigParams.config)
                     ]
                        ++ configToBorderStyle borderConfigParams.config
                    )
                    subImages

            adjustColor =
                Dict.get colorConfigParams.config colorConfigParams.memoized
                    |> Maybe.map .adjust
                    |> Maybe.withDefault
                        { tl = colorConfigParams.adjustments.tl colorConfigParams.config
                        , tr = colorConfigParams.adjustments.tr colorConfigParams.config
                        , bl = colorConfigParams.adjustments.bl colorConfigParams.config
                        , br = colorConfigParams.adjustments.br colorConfigParams.config
                        }

            adjustBorder =
                Dict.get borderConfigParams.config borderConfigParams.memoized
                    |> Maybe.map .adjust
                    |> Maybe.withDefault
                        { tl = borderConfigParams.adjustments.tl borderConfigParams.config
                        , tr = borderConfigParams.adjustments.tr borderConfigParams.config
                        , bl = borderConfigParams.adjustments.bl borderConfigParams.config
                        , br = borderConfigParams.adjustments.br borderConfigParams.config
                        }

            newColorConfigParams =
                { colorConfigParams | memoized = Dict.insert colorConfigParams.config { adjust = adjustColor } colorConfigParams.memoized }

            newBorderConfigParams =
                { borderConfigParams | memoized = Dict.insert borderConfigParams.config { adjust = adjustBorder } borderConfigParams.memoized }

            ( tlImage, colorMemoized2, borderMemoized2 ) =
                generateImage
                    { newColorConfigParams | config = adjustColor.tl }
                    { newBorderConfigParams | config = adjustBorder.tl }
                    (level - 1)
                    (pathKey ++ "-tl")
                    "tl"

            ( trImage, colorMemoized3, borderMemoized3 ) =
                generateImage
                    { colorMemoized2 | config = adjustColor.tr }
                    { borderMemoized2 | config = adjustBorder.tr }
                    (level - 1)
                    (pathKey ++ "-tr")
                    "tr"

            ( blImage, colorMemoized4, borderMemoized4 ) =
                generateImage
                    { colorMemoized3 | config = adjustColor.bl }
                    { borderMemoized3 | config = adjustBorder.bl }
                    (level - 1)
                    (pathKey ++ "-bl")
                    "bl"

            ( brImage, colorMemoized5, borderMemoized5 ) =
                generateImage
                    { colorMemoized4 | config = adjustColor.br }
                    { borderMemoized4 | config = adjustBorder.br }
                    (level - 1)
                    (pathKey ++ "-br")
                    "br"
        in
        ( wrapImages
            [ ( pathKey ++ "-tl", tlImage )
            , ( pathKey ++ "-tr", trImage )
            , ( pathKey ++ "-bl", blImage )
            , ( pathKey ++ "-br", brImage )
            ]
        , colorMemoized5
        , borderMemoized5
        )


randomListShuffleFunction : Int -> Random.Generator (List Int -> List Int)
randomListShuffleFunction listLength =
    Random.List.shuffle (List.range 0 (listLength - 1))
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
    [ ( String.fromInt model.iteration
      , div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "left" "0"
            ]
            [ generateImage
                { adjustments = model.adjustments
                , memoized = Dict.empty
                , config = model.initialVariables
                }
                { adjustments = model.borderAdjustments
                , memoized = Dict.empty
                , config = [ 0, 1, 2, 3 ]
                }
                maxLevel
                ("level-" ++ String.fromInt maxLevel)
                "outer"
                |> (\( image, _, _ ) -> image)
            ]
      )
    ]


cssStyles : String
cssStyles =
    """
div {
    box-sizing: border-box;
    overflow: hidden;
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
            , onClick
                Randomize
            ]
            (viewFrameworks model)
        ]



--


type alias Model =
    { iteration : Int
    , adjustments : Adjustments Config
    , borderAdjustments : Adjustments Config
    , initialVariables : Config
    , randomSeed : Random.Seed
    , numberOfVariables : Int -- Length of list
    , doNextAnimationFrame : List Msg
    }


maxLevel : Int
maxLevel =
    7


init : Random.Seed -> ( Model, Cmd Msg )
init seed =
    let
        numberOfVariables =
            4

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( borderAdjustments, seedAfterBorderAdjustments ) =
            Random.step (randomizeAdjustments 4) seedAfterAdustments

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomVariables numberOfVariables) seedAfterBorderAdjustments
    in
    ( { iteration = 0
      , adjustments = adjustments
      , borderAdjustments = borderAdjustments
      , initialVariables = newInitialColor
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      , doNextAnimationFrame = []
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | GotNextAnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            let
                ( randomizedAdjustments, seedAfterAdustments ) =
                    Random.step (randomizeAdjustments model.numberOfVariables) model.randomSeed

                ( borderAdjustments, seedAfterBorderAdjustments ) =
                    Random.step (randomizeAdjustments 4) seedAfterAdustments

                ( newInitialColor, newSeed ) =
                    Random.step (randomVariables model.numberOfVariables) seedAfterBorderAdjustments
            in
            ( { model
                | adjustments = randomizedAdjustments
                , borderAdjustments = borderAdjustments
                , initialVariables = newInitialColor
                , randomSeed = newSeed
                , iteration = model.iteration + 1
              }
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


subscriptions : Model -> Sub Msg
subscriptions { doNextAnimationFrame } =
    if List.isEmpty doNextAnimationFrame then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta
            (\_ ->
                GotNextAnimationFrame
            )
