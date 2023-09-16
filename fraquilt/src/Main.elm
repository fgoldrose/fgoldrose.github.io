module Main exposing (Flags, FraquiltVariety, Model(..), Msg(..), main)

import Basic
import Browser
import Css
import FadeBorders
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as HE
import Leafy
import Random
import Spirally
import Wiggly
import Windows


type FraquiltVariety
    = Leafy
    | Wiggly
    | Windows
    | Spirally
    | FadeBorders
    | Basic


type Model
    = LeafyModel Leafy.Model
    | WigglyModel Wiggly.Model
    | WindowsModel Windows.Model
    | SpirallyModel Spirally.Model
    | FadeBordersModel FadeBorders.Model
    | BasicModel Basic.Model


type Msg
    = ChangeVariety FraquiltVariety
    | LeafyMsg Leafy.Msg
    | WigglyMsg Wiggly.Msg
    | WindowsMsg Windows.Msg
    | SpirallyMsg Spirally.Msg
    | FadeBordersMsg FadeBorders.Msg
    | BasicMsg Basic.Msg


isVariety : FraquiltVariety -> Model -> Bool
isVariety variety model =
    case ( variety, model ) of
        ( Leafy, LeafyModel _ ) ->
            True

        ( Wiggly, WigglyModel _ ) ->
            True

        ( Windows, WindowsModel _ ) ->
            True

        ( Spirally, SpirallyModel _ ) ->
            True

        ( FadeBorders, FadeBordersModel _ ) ->
            True

        ( Basic, BasicModel _ ) ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    let
        controls =
            Html.div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.padding (Css.px 10)
                    , Css.backgroundColor (Css.hex "000000")
                    , Css.fontFamily Css.sansSerif
                    , Css.fontWeight Css.bold
                    , Css.color (Css.hex "ffffff")
                    ]
                ]
                (Html.div
                    [ css [ Css.marginBottom (Css.px 10) ] ]
                    [ Html.text "Change type:" ]
                    :: List.map
                        (\( variety, label ) ->
                            Html.button
                                [ css
                                    [ if isVariety variety model then
                                        Css.backgroundColor (Css.hex "555555")

                                      else
                                        Css.backgroundColor (Css.hex "333333")
                                    , Css.color (Css.hex "ffffff")
                                    , Css.padding (Css.px 10)
                                    , Css.hover [ Css.backgroundColor (Css.hex "555555") ]
                                    , Css.border (Css.px 0)
                                    , Css.cursor Css.pointer
                                    , Css.borderRadius (Css.px 8)
                                    , Css.marginBottom (Css.px 10)
                                    ]
                                , HE.onClick (ChangeVariety variety)
                                ]
                                [ Html.text label ]
                        )
                        [ ( Basic, "Basic" )
                        , ( FadeBorders, "Borders" )
                        , ( Windows, "Windows" )
                        , ( Wiggly, "Wiggly" )
                        , ( Leafy, "Leafy" )
                        , ( Spirally, "Spirally" )
                        ]
                )

        mainImage =
            case model of
                LeafyModel subModel ->
                    Leafy.view subModel |> Html.map LeafyMsg

                WigglyModel subModel ->
                    Wiggly.view subModel |> Html.map WigglyMsg

                WindowsModel subModel ->
                    Windows.view subModel |> Html.map WindowsMsg

                SpirallyModel subModel ->
                    Spirally.view subModel |> Html.map SpirallyMsg

                FadeBordersModel subModel ->
                    FadeBorders.view subModel |> Html.map FadeBordersMsg

                BasicModel subModel ->
                    Basic.view subModel |> Html.map BasicMsg
    in
    Html.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.position Css.absolute
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.right (Css.px 0)
            , Css.bottom (Css.px 0)
            ]
        ]
        [ controls
        , Html.div
            [ css
                [ Css.position Css.relative
                , Css.flexGrow (Css.num 1)
                , Css.cursor Css.pointer
                ]
            ]
            [ mainImage ]
        ]


init : FraquiltVariety -> Random.Seed -> ( Model, Cmd Msg )
init variety seed =
    case variety of
        Leafy ->
            Leafy.init seed
                |> Tuple.mapBoth LeafyModel (Cmd.map LeafyMsg)

        Wiggly ->
            Wiggly.init seed
                |> Tuple.mapBoth WigglyModel (Cmd.map WigglyMsg)

        Windows ->
            Windows.init seed
                |> Tuple.mapBoth WindowsModel (Cmd.map WindowsMsg)

        Spirally ->
            Spirally.init seed
                |> Tuple.mapBoth SpirallyModel (Cmd.map SpirallyMsg)

        FadeBorders ->
            FadeBorders.init seed
                |> Tuple.mapBoth FadeBordersModel (Cmd.map FadeBordersMsg)

        Basic ->
            Basic.init seed
                |> Tuple.mapBoth BasicModel (Cmd.map BasicMsg)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LeafyModel subModel ->
            Leafy.subscriptions subModel |> Sub.map LeafyMsg

        WigglyModel subModel ->
            Wiggly.subscriptions subModel |> Sub.map WigglyMsg

        WindowsModel subModel ->
            Windows.subscriptions subModel |> Sub.map WindowsMsg

        SpirallyModel subModel ->
            Spirally.subscriptions subModel |> Sub.map SpirallyMsg

        FadeBordersModel subModel ->
            FadeBorders.subscriptions subModel |> Sub.map FadeBordersMsg

        BasicModel subModel ->
            Basic.subscriptions subModel |> Sub.map BasicMsg


getRandomSeed : Model -> Random.Seed
getRandomSeed model =
    case model of
        LeafyModel subModel ->
            subModel.randomSeed

        WigglyModel subModel ->
            subModel.randomSeed

        WindowsModel subModel ->
            subModel.randomSeed

        SpirallyModel subModel ->
            subModel.randomSeed

        FadeBordersModel subModel ->
            subModel.randomSeed

        BasicModel subModel ->
            subModel.randomSeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangeVariety variety, _ ) ->
            init variety (getRandomSeed model)

        ( LeafyMsg subMsg, LeafyModel subModel ) ->
            Leafy.update subMsg subModel
                |> Tuple.mapBoth LeafyModel (Cmd.map LeafyMsg)

        ( WigglyMsg subMsg, WigglyModel subModel ) ->
            Wiggly.update subMsg subModel
                |> Tuple.mapBoth WigglyModel (Cmd.map WigglyMsg)

        ( WindowsMsg subMsg, WindowsModel subModel ) ->
            Windows.update subMsg subModel
                |> Tuple.mapBoth WindowsModel (Cmd.map WindowsMsg)

        ( SpirallyMsg subMsg, SpirallyModel subModel ) ->
            Spirally.update subMsg subModel
                |> Tuple.mapBoth SpirallyModel (Cmd.map SpirallyMsg)

        ( FadeBordersMsg subMsg, FadeBordersModel subModel ) ->
            FadeBorders.update subMsg subModel
                |> Tuple.mapBoth FadeBordersModel (Cmd.map FadeBordersMsg)

        ( BasicMsg subMsg, BasicModel subModel ) ->
            Basic.update subMsg subModel
                |> Tuple.mapBoth BasicModel (Cmd.map BasicMsg)

        _ ->
            ( model, Cmd.none )


type alias Flags =
    { randomSeed : Int }


main : Program Flags Model Msg
main =
    Browser.element
        { init =
            \flags ->
                flags.randomSeed
                    |> Random.initialSeed
                    |> init Basic
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
