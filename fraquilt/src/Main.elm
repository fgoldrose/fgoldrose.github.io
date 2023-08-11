module Main exposing (Flags, Model(..), Msg(..), main)

import Browser
import Html exposing (Html)
import Leafy


type Model
    = Leafy Leafy.Model


type Msg
    = LeafyMsg Leafy.Msg


view : Model -> Html Msg
view model =
    case model of
        Leafy subModel ->
            Leafy.view subModel |> Html.map LeafyMsg


type alias Flags =
    { randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    Leafy.init flags
        |> Tuple.mapBoth Leafy (Cmd.map LeafyMsg)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Leafy subModel ->
            Leafy.subscriptions subModel |> Sub.map LeafyMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LeafyMsg subMsg, Leafy subModel ) ->
            Leafy.update subMsg subModel
                |> Tuple.mapBoth Leafy (Cmd.map LeafyMsg)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
