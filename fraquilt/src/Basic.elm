module Basic exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import Css
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Json.Decode
import Random
import Task
import Utils exposing (Adjustments, Config, Direction(..), configToRbgString, cssStyles, randomVariables, randomizeAdjustments)


generateImage : Adjustments Config -> Int -> String -> String -> Config -> Html msg
generateImage adjustments level pathKey currentPosition config =
    if level == 0 then
        div
            [ class "box"
            , class currentPosition
            , id pathKey
            , css [ Css.backgroundColor (configToRbgString config) ]
            ]
            []

    else
        Keyed.node "div"
            [ class "box", class currentPosition ]
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


viewFrameworks : Model -> List ( String, Html Msg )
viewFrameworks model =
    List.range 0 maxLevel
        |> List.map
            (\level ->
                ( String.fromInt level
                , div
                    [ id ("level-" ++ String.fromInt level)
                    , css
                        [ Css.opacity
                            (if level <= model.level then
                                Css.num 1

                             else
                                Css.num 0
                            )
                        ]
                    , class "container"
                    , Html.Styled.Attributes.style "transition" "opacity 0.5s linear"
                    , if level == 0 then
                        onTransitionEnd Randomize

                      else
                        class ""
                    ]
                    [ Lazy.lazy5 generateImage
                        model.adjustments
                        level
                        ("level-" ++ String.fromInt level)
                        "outer"
                        model.initialVariables
                    ]
                )
            )


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ Html.text cssStyles ]
        , Keyed.node "div"
            [ class "container"
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


maxLevel : Int
maxLevel =
    7


init : Random.Seed -> ( Model, Cmd Msg )
init seed =
    let
        numberOfVariables =
            6

        level =
            0

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
    Html.Styled.Events.on "transitionend" (Json.Decode.succeed msg)


subscriptions : Model -> Sub Msg
subscriptions { doNextAnimationFrame } =
    if List.isEmpty doNextAnimationFrame then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta
            (\_ ->
                GotNextAnimationFrame
            )
