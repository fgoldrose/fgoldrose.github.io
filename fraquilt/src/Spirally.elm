module Spirally exposing (Memoized, Model, Msg(..), init, subscriptions, update, view)

import Css
import Dict
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Random
import Utils exposing (Adjustments, Config, configToRbgString, cssStyles, randomVariables, randomizeAdjustments)


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
                , Utils.configToBorderRadius (List.drop 3 config)
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
                            , css [ Utils.configToBorderRadius (List.drop 3 config) ]
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


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ Html.text cssStyles ]
        , Keyed.node "div"
            [ class "container"
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
