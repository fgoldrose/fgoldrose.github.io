module Leafy exposing (..)

import Css
import Dict
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Random
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

        -- , levelImages : Dict.Dict Int (List (Html Msg))
        }


type alias ConfigParams =
    { config : Config
    , adjustments : Adjustments Config
    , memoized : Memoized
    }


generateImage : ConfigParams -> ConfigParams -> Int -> String -> Css.Style -> ( Html Msg, ConfigParams, ConfigParams )
generateImage colorConfigParams borderConfigParams level pathKey currentPosition =
    if level == 0 then
        ( div
            [ css
                [ Utils.boxStyle
                , currentPosition
                , Css.backgroundColor (configToRbgString colorConfigParams.config)
                , configToBorderStyle borderConfigParams.config
                ]
            , id pathKey
            ]
            []
        , colorConfigParams
        , borderConfigParams
        )

    else
        let
            wrapImages subImages =
                Keyed.node "div"
                    [ css
                        [ Css.backgroundColor (configToRbgString colorConfigParams.config)
                        , boxStyle
                        , currentPosition
                        ]
                    ]
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
                    tlStyle

            ( trImage, colorMemoized3, borderMemoized3 ) =
                generateImage
                    { colorMemoized2 | config = adjustColor.tr }
                    { borderMemoized2 | config = adjustBorder.tr }
                    (level - 1)
                    (pathKey ++ "-tr")
                    trStyle

            ( blImage, colorMemoized4, borderMemoized4 ) =
                generateImage
                    { colorMemoized3 | config = adjustColor.bl }
                    { borderMemoized3 | config = adjustBorder.bl }
                    (level - 1)
                    (pathKey ++ "-bl")
                    blStyle

            ( brImage, colorMemoized5, borderMemoized5 ) =
                generateImage
                    { colorMemoized4 | config = adjustColor.br }
                    { borderMemoized4 | config = adjustBorder.br }
                    (level - 1)
                    (pathKey ++ "-br")
                    brStyle
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


randomizeColors : Int -> Random.Seed -> ( ConfigParams, Random.Seed )
randomizeColors numberOfVariables seed =
    let
        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, newSeed ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments
    in
    ( { adjustments = adjustments, config = newInitialColor, memoized = Dict.empty }
    , newSeed
    )


randomizeBorder : Int -> Random.Seed -> ( ConfigParams, Random.Seed )
randomizeBorder numberOfVariables seed =
    let
        ( borderAdjustments, seed1 ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitial, seed2 ) =
            Random.step (Random.list 4 (Random.int 0 100)) seed1
    in
    ( { adjustments = borderAdjustments, config = newInitial, memoized = Dict.empty }
    , seed2
    )


viewFrameworks : Model -> List ( String, Html Msg )
viewFrameworks model =
    [ ( String.fromInt model.iteration
      , div
            [ css [ containerStyle ] ]
            [ generateImage
                model.colorParams
                model.borderParams
                maxLevel
                ("level-" ++ String.fromInt maxLevel)
                outerStyle
                |> (\( image, _, _ ) -> image)
            ]
      )
    ]


view : Model -> Html Msg
view model =
    div []
        [ Keyed.node "div"
            [ css
                [ containerStyle
                ]
            , onClick
                Randomize
            ]
            (viewFrameworks model)
        ]



--


type alias Model =
    { iteration : Int
    , colorParams : ConfigParams
    , borderParams : ConfigParams
    , randomSeed : Random.Seed
    , numberOfVariables : Int -- Length of list
    }


maxLevel : Int
maxLevel =
    6


init : Random.Seed -> ( Model, Cmd Msg )
init randomSeed =
    let
        numberOfVariables =
            4

        ( colorParams, seed1 ) =
            randomizeColors numberOfVariables randomSeed

        ( borderParams, seed2 ) =
            randomizeBorder 4 seed1
    in
    ( { iteration = 0
      , colorParams = colorParams
      , borderParams = borderParams
      , randomSeed = seed2
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
                ( newColorParams, seed1 ) =
                    randomizeColors model.numberOfVariables model.randomSeed

                ( newBorderParams, seed2 ) =
                    randomizeBorder 4 seed1

                ( _, memoizeColors, memoizeBorders ) =
                    generateImage newColorParams
                        newBorderParams
                        maxLevel
                        ("level-" ++ String.fromInt maxLevel)
                        (Css.batch
                            [ Css.position Css.absolute
                            , Css.width (Css.pct 100)
                            , Css.height (Css.pct 100)
                            ]
                        )
            in
            ( { model
                | colorParams = memoizeColors
                , borderParams = memoizeBorders
                , randomSeed = seed2
                , iteration = model.iteration + 1
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
