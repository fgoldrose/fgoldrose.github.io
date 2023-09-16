module Utils exposing (..)

import Css
import List.Extra as List
import Random
import Random.List


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }


configToRbgString : List Int -> Css.Color
configToRbgString list =
    case list of
        r :: g :: b :: _ ->
            Css.rgb r g b

        _ ->
            Css.rgb 0 0 0


configToBorderStyle : List Int -> Css.Style
configToBorderStyle list =
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


boxStyle : Css.Style
boxStyle =
    Css.batch
        [ Css.height (Css.pct 50)
        , Css.width (Css.pct 50)
        , Css.position Css.absolute
        ]


containerStyle : Css.Style
containerStyle =
    Css.batch
        [ Css.position Css.absolute
        , Css.top (Css.px 0)
        , Css.left (Css.px 0)
        , Css.right (Css.px 0)
        , Css.bottom (Css.px 0)
        ]


outerStyle : Css.Style
outerStyle =
    Css.batch
        [ Css.position Css.relative
        , Css.height (Css.pct 100)
        , Css.width (Css.pct 100)
        ]


tlStyle : Css.Style
tlStyle =
    Css.batch
        [ Css.top (Css.px 0)
        , Css.left (Css.px 0)
        ]


trStyle : Css.Style
trStyle =
    Css.batch
        [ Css.top (Css.px 0)
        , Css.right (Css.px 0)
        ]


blStyle : Css.Style
blStyle =
    Css.batch
        [ Css.bottom (Css.px 0)
        , Css.left (Css.px 0)
        ]


brStyle : Css.Style
brStyle =
    Css.batch
        [ Css.bottom (Css.px 0)
        , Css.right (Css.px 0)
        ]



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
