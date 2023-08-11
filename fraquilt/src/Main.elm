module Main exposing (Flags, Model(..), Msg(..), main)

import Browser
import Html exposing (Html)
import Leafy
import Spirally
import Wiggly
import Windows


type FraquiltVariety
    = Leafy
    | Wiggly
    | Windows
    | Spirally


type Model
    = LeafyModel Leafy.Model
    | WigglyModel Wiggly.Model
    | WindowsModel Windows.Model
    | SpirallyModel Spirally.Model


type Msg
    = LeafyMsg Leafy.Msg
    | WigglyMsg Wiggly.Msg
    | WindowsMsg Windows.Msg
    | SpirallyMsg Spirally.Msg


view : Model -> Html Msg
view model =
    case model of
        LeafyModel subModel ->
            Leafy.view subModel |> Html.map LeafyMsg

        WigglyModel subModel ->
            Wiggly.view subModel |> Html.map WigglyMsg

        WindowsModel subModel ->
            Windows.view subModel |> Html.map WindowsMsg

        SpirallyModel subModel ->
            Spirally.view subModel |> Html.map SpirallyMsg


type alias Flags =
    { randomSeed : Int }


init : FraquiltVariety -> Flags -> ( Model, Cmd Msg )
init variety flags =
    case variety of
        Leafy ->
            Leafy.init flags
                |> Tuple.mapBoth LeafyModel (Cmd.map LeafyMsg)

        Wiggly ->
            Wiggly.init flags
                |> Tuple.mapBoth WigglyModel (Cmd.map WigglyMsg)

        Windows ->
            Windows.init flags
                |> Tuple.mapBoth WindowsModel (Cmd.map WindowsMsg)

        Spirally ->
            Spirally.init flags
                |> Tuple.mapBoth SpirallyModel (Cmd.map SpirallyMsg)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
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

        _ ->
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init Spirally
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
