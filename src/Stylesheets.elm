port module Stylesheets exposing (..)

import Css exposing (..)
import Css.Colors as C
import Css.Elements as E
import Css.File
import Css.Media as Media
import ModularScale
import Types exposing (..)


port files : Css.File.CssFileStructure -> Cmd msg


cssFiles : Css.File.CssFileStructure
cssFiles =
    Css.File.toFileStructure [ ( "main.css", Css.compile [ css ] ) ]


main : Css.File.CssCompilerProgram
main =
    Css.File.compiler files cssFiles


css : Stylesheet
css =
    Css.stylesheet <|
        List.concat
            [ typography
            , inputs
            , headers
            , containers
            , searchView
            , pageView
            , alternateView
            ]



-- TYPOGRAPHY


typography : List Css.Snippet
typography =
    [ E.h1
        [ padding2 (ms 1) zero
        , margin zero
        , fontSize (ms 7)
        , fontWeight (int 500)
        ]
    , E.h2
        [ padding2 (ms -1) zero
        , margin zero
        , fontSize (ms 4)
        , lineHeight (ms 5)
        , fontWeight (int 500)
        ]
    , E.h3
        [ padding2 (ms -1) zero
        , margin zero
        , fontSize (ms 2)
        , lineHeight (ms 3)
        , fontWeight (int 400)
        ]
    , E.p
        [ color (mono B3)
        , margin zero
        , fontSize (ms 1)
        , lineHeight (ms 3)
        ]
    , E.a
        [ color (mono B1)
        , textDecoration none
        ]
    , E.ul
        [ padding zero
        , margin zero
        , listStyle none
        ]
    ]



-- HEADERS


headers : List Css.Snippet
headers =
    [ class SiteLogo
        [ flexBasis (pct 100)
        , margin2 (ms 4) zero
        ]
    , E.header
        [ displayFlex
        , justifyContent center
        , flexWrap wrap
        , textAlign center
        , descendants
            [ E.a [ flexBasis (pct 100) ] ]
        ]
    , E.form
        [ position relative
        , displayFlex
        , marginBottom (ms 5)
        , backgroundColor (mono W1)
        , border3 (px 2) solid (mono W4)
        ]
    , E.button
        [ width (ms 8)
        , displayFlex
        , alignItems center
        , backgroundColor (mono W1)
        , border zero
        , justifyContent center
        , outline none
        , marginTop (px 2)
        , cursor pointer
        ]
    , class CategoryList
        [ displayFlex
        , width (pct 100)
        , justifyContent center
        , descendants
            [ E.li
                [ margin2 zero (ms 0)
                , fontSize (ms 1)
                , textTransform capitalize
                , cursor pointer
                , padding2 (ms -1) (ms -2)
                , borderRadius (px 4)
                ]
            , class "selected"
                [ backgroundColor C.yellow
                ]
            ]
        ]
    ]



-- INPUTS


inputs : List Css.Snippet
inputs =
    [ E.input
        [ border zero
        , padding (ms 1)
        , fontSize (ms 3)
        , outline none
        , width (pct 100)
        ]
    ]



-- CONTAINERS


containers : List Css.Snippet
containers =
    [ E.html
        [ fontSize (pct <| 100 * 12 / 16)
        ]
    , E.body
        [ margin zero
        , padding zero
        , fontFamilies sans
        , backgroundColor (mono W3)
        ]
    , E.img
        [ width (pct 100)
        , minHeight (ms 8)
        , borderRadius (px 4)
        , backgroundColor (mono W4)
        ]
    ]


searchView : List Css.Snippet
searchView =
    [ class SearchView
        [ padding (ms 6)
        , descendants
            [ E.ul
                [ property "column-count" "2"
                , property "column-gap" "2rem"
                , smallDisplay [ property "column-count" "4" ]
                , largeDisplay [ property "column-count" "6" ]
                ]
            ]
        ]
    , class SearchViewResult
        [ marginBottom (ms 3)
        , backgroundColor (mono W1)
        , property "break-inside" "avoid-column"
        , descendants
            [ E.h3
                [ padding (ms 1)
                ]
            ]
        ]
    ]


pageView : List Css.Snippet
pageView =
    [ class PageView
        [ maxWidth (ms 18)
        , margin2 zero auto
        , padding (ms 6)
        ]
    ]


alternateViewStyle : Style
alternateViewStyle =
    batch
        [ color (mono G1)
        , fontSize (ms 3)
        , property "-webkit-font-smoothing" "antialiased"
        , textAlign center
        , paddingTop (ms 5)
        , margin2 zero auto
        ]


alternateView : List Css.Snippet
alternateView =
    [ class NotificationView [ alternateViewStyle ]
    ]



-- MODULARSCALE


config : ModularScale.Config
config =
    { base = [ 1, 1.2 ]
    , interval = ModularScale.PerfectFifth
    }


ms : Int -> Rem
ms =
    Css.rem << ModularScale.get config


smallDisplay : List Style -> Style
smallDisplay =
    Media.withMediaQuery [ "screen and (min-width: 768px)" ]


largeDisplay : List Style -> Style
largeDisplay =
    Media.withMediaQuery [ "screen and (min-width: 1280px)" ]



-- VARIABLES


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type Mono
    = B1
    | B2
    | B3
    | B4
    | W1
    | W2
    | W3
    | W4
    | G1
    | G2
    | G3
    | G4


monoValues : List ( Mono, Int )
monoValues =
    [ B1 => 6
    , B2 => 18
    , B3 => 26
    , B4 => 30
    , W1 => 255
    , W2 => 249
    , W3 => 243
    , W4 => 237
    , G1 => 164
    , G2 => 188
    , G3 => 212
    , G4 => 224
    ]


mono : Mono -> Color
mono v =
    case List.filter ((==) v << Tuple.first) monoValues of
        [ ( x, y ) ] ->
            rgb y y y

        _ ->
            rgba 0 0 0 0


sans : List String
sans =
    [ "-apple-system"
    , "BlinkMacSystemFont"
    , "Segoe UI"
    , "Roboto"
    , "Oxygen"
    , "Ubuntu"
    , "Cantarell"
    , "Fira Sans"
    , "Droid Sans"
    , "Helvetica Neue"
    , "sans-serif"
    ]
