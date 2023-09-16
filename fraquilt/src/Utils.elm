module Utils exposing (Adjustments, Config, Direction(..), configToBorderRadius, configToBorderWidth, configToRbgString, cssStyles, randomVariables, randomizeAdjustments)

import Css
import List.Extra as List
import Random
import Random.List


type alias Config =
    List Int


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }


type Direction
    = Up
    | Down
    | None


configToRbgString : List Int -> Css.Color
configToRbgString list =
    case list of
        r :: g :: b :: _ ->
            Css.rgb r g b

        _ ->
            Css.rgb 0 0 0


configToBorderRadius : List Int -> Css.Style
configToBorderRadius list =
    case list of
        l :: r :: t :: b :: _ ->
            Css.batch
                [ Css.borderTopLeftRadius (Css.pct (toFloat l))
                , Css.borderBottomRightRadius (Css.pct (toFloat r))
                , Css.borderTopRightRadius (Css.pct (toFloat t))
                , Css.borderBottomLeftRadius (Css.pct (toFloat b))
                ]

        _ ->
            Css.batch []


configToBorderWidth : List Int -> Css.Style
configToBorderWidth list =
    case list of
        l :: r :: t :: b :: _ ->
            Css.batch
                [ Css.borderTopWidth (Css.px (toFloat t))
                , Css.borderBottomWidth (Css.px (toFloat b))
                , Css.borderLeftWidth (Css.px (toFloat l))
                , Css.borderRightWidth (Css.px (toFloat r))
                ]

        _ ->
            Css.batch []


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

.container {
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



-- Randomize


randomizeAdjustments : Int -> Random.Generator (Adjustments (List Int))
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


randomVariables : Int -> Random.Generator (List Int)
randomVariables n =
    Random.list n (Random.int 0 255)
