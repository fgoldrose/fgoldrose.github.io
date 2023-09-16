module Wiggly exposing (ConfigParams, Memoized, Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import Css
import Dict
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Random
import Random.List
import Utils exposing (Adjustments, Config, configToBorderWidth, configToRbgString, cssStyles, randomVariables, randomizeAdjustments)


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
            , css [ Css.backgroundColor (configToRbgString colorConfigParams.config) ]
            ]
            []
        , colorConfigParams
        , borderConfigParams
        )

    else
        let
            wrapImages subImages =
                Keyed.node "div"
                    [ class "box"
                    , class currentPosition
                    , css
                        [ Css.borderStyle Css.solid
                        , Css.borderColor (configToRbgString colorConfigParams.config)
                        , Css.backgroundColor (configToRbgString colorConfigParams.config)
                        , configToBorderWidth borderConfigParams.config
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


viewFrameworks : Model -> List ( String, Html Msg )
viewFrameworks model =
    [ ( String.fromInt model.iteration
      , div
            [ class "container"
            ]
            [ generateImage
                model.colorParams
                model.borderParams
                maxLevel
                ("level-" ++ String.fromInt maxLevel)
                "outer"
                |> (\( image, _, _ ) -> image)
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
    , colorParams : ConfigParams
    , borderParams : ConfigParams
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
            4

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( borderAdjustments, seedAfterBorderAdjustments ) =
            Random.step (randomizeAdjustments 4) seedAfterAdustments

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomVariables numberOfVariables) seedAfterBorderAdjustments
    in
    ( { iteration = 0
      , colorParams = { adjustments = adjustments, config = newInitialColor, memoized = Dict.empty }
      , borderParams = { adjustments = borderAdjustments, config = [ 1, 2, 3, 4 ], memoized = Dict.empty }
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | RandomizeBorder


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

                newColorParams =
                    { adjustments = randomizedAdjustments, config = newInitialColor, memoized = Dict.empty }

                newBorderParams =
                    { adjustments = borderAdjustments, config = [ 1, 2, 3, 4 ], memoized = Dict.empty }

                ( _, memoizeColors, memoizeBorders ) =
                    generateImage newColorParams newBorderParams maxLevel ("level-" ++ String.fromInt maxLevel) "outer"
            in
            ( { model
                | colorParams = memoizeColors
                , borderParams = memoizeBorders
                , randomSeed = newSeed
                , iteration = model.iteration + 1
              }
            , Cmd.none
            )

        RandomizeBorder ->
            let
                ( borderAdjustments, seed1 ) =
                    Random.step (randomizeAdjustments 4) model.randomSeed

                ( newInitial, seed2 ) =
                    Random.step (Random.List.shuffle [ 1, 2, 3, 4 ]) seed1
            in
            ( { model
                | borderParams = { adjustments = borderAdjustments, config = newInitial, memoized = Dict.empty }
                , randomSeed = seed2
                , iteration = model.iteration + 1
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame (\_ -> RandomizeBorder)
