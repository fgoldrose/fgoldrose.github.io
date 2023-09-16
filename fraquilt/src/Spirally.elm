module Spirally exposing (..)

import Css
import Dict
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import List.Extra as List
import Random
import Random.List
import Utils exposing (..)


type alias Config =
    List Int


type alias Memoized =
    Dict.Dict Config
        { adjust :
            { tl : Config
            , tr : Config
            , bl : Config
            , br : Config
            }
        , levelImages : Dict.Dict Int (Html Msg)
        }


generateImage : Adjustments Config -> Memoized -> Int -> String -> String -> Config -> ( Html Msg, Memoized )
generateImage adjustments memoized level pathKey currentPosition config =
    if level == 0 then
        ( div
            [ class "box"
            , class currentPosition
            , id pathKey
            , css
                [ Css.backgroundColor (configToRbgString config)
                , Utils.configToBorderStyle (List.drop 3 config)
                ]
            ]
            []
        , memoized
        )

    else
        let
            wrapImages subImages =
                Keyed.node "div"
                    [ class "box"
                    , class currentPosition
                    , css [ Css.backgroundColor (configToRbgString config) ]
                    ]
                    [ ( pathKey ++ "-outer"
                      , Keyed.node "div"
                            [ class "outer"
                            , css [ Utils.configToBorderStyle (List.drop 3 config) ]
                            ]
                            subImages
                      )
                    ]

            generateImageLevel configs =
                let
                    ( tlImage, memoized2 ) =
                        generateImage adjustments
                            memoized
                            (level - 1)
                            (pathKey ++ "-tl")
                            "tl"
                            configs.tl

                    ( trImage, memoized3 ) =
                        generateImage adjustments
                            memoized2
                            (level - 1)
                            (pathKey ++ "-tr")
                            "tr"
                            configs.tr

                    ( blImage, memoized4 ) =
                        generateImage adjustments
                            memoized3
                            (level - 1)
                            (pathKey ++ "-bl")
                            "bl"
                            configs.bl

                    ( brImage, memoized5 ) =
                        generateImage adjustments
                            memoized4
                            (level - 1)
                            (pathKey ++ "-br")
                            "br"
                            configs.br
                in
                ( wrapImages
                    [ ( pathKey ++ "-tl", tlImage )
                    , ( pathKey ++ "-tr", trImage )
                    , ( pathKey ++ "-bl", blImage )
                    , ( pathKey ++ "-br", brImage )
                    ]
                , memoized5
                )
        in
        case Dict.get config memoized of
            Just { adjust, levelImages } ->
                case Dict.get level levelImages of
                    Just image ->
                        ( image, memoized )

                    Nothing ->
                        let
                            ( image, returnedMemoized ) =
                                generateImageLevel adjust

                            newMemoized =
                                Dict.insert config
                                    { adjust = adjust
                                    , levelImages = Dict.insert level image levelImages
                                    }
                                    returnedMemoized
                        in
                        ( image, newMemoized )

            Nothing ->
                let
                    adjust =
                        { tl = adjustments.tl config
                        , tr = adjustments.tr config
                        , bl = adjustments.bl config
                        , br = adjustments.br config
                        }

                    ( image, returnedMemoized ) =
                        generateImageLevel adjust

                    newMemoized =
                        Dict.insert config
                            { adjust = adjust
                            , levelImages = Dict.singleton level image
                            }
                            returnedMemoized
                in
                ( image, newMemoized )


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
            [ css
                [ Css.position Css.absolute
                , Css.top (Css.px 0)
                , Css.bottom (Css.px 0)
                , Css.left (Css.px 0)
                , Css.right (Css.px 0)
                ]
            ]
            [ generateImage
                model.adjustments
                Dict.empty
                maxLevel
                ("level-" ++ String.fromInt maxLevel)
                "outer"
                model.initialVariables
                |> Tuple.first
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
    , initialVariables : Config
    , randomSeed : Random.Seed
    , numberOfVariables : Int -- Length of list
    }


maxLevel : Int
maxLevel =
    6


init : Random.Seed -> ( Model, Cmd Msg )
init seed =
    let
        numberOfVariables =
            7

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments
    in
    ( { iteration = 0
      , adjustments = adjustments
      , initialVariables = newInitialColor
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      }
    , Cmd.none
    )


type Msg
    = Randomize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
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
                , iteration = model.iteration + 1
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
