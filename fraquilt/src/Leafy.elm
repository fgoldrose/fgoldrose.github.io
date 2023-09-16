module Leafy exposing (ConfigParams, Memoized, Model, Msg(..), init, subscriptions, update, view)

import Css
import Dict
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Random
import Utils exposing (Adjustments, Config, configToBorderRadius, configToRbgString, cssStyles, randomVariables, randomizeAdjustments)


type alias Memoized =
    Dict.Dict Config
        { adjust :
            { tl : Config
            , tr : Config
            , bl : Config
            , br : Config
            }
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
            , css [ Css.backgroundColor (configToRbgString colorConfigParams.config), configToBorderRadius borderConfigParams.config ]
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
                    , css [ Css.backgroundColor (configToRbgString colorConfigParams.config) ]
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


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ Html.text cssStyles ]
        , Keyed.node "div"
            [ class "container"
            , onClick
                Randomize
            ]
            [ ( String.fromInt model.iteration
              , generateImage
                    model.colorParams
                    model.borderParams
                    maxLevel
                    ("level-" ++ String.fromInt maxLevel)
                    "outer"
                    |> (\( image, _, _ ) -> image)
              )
            ]
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
                    generateImage newColorParams newBorderParams maxLevel ("level-" ++ String.fromInt maxLevel) "outer"
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
