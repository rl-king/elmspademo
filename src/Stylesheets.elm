port module Stylesheets exposing (..)

import Css exposing (..)
import Css.Elements as E
import Css.File
import ModularScale
import Types


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
            ]



-- TYPOGRAPHY


typography : List Css.Snippet
typography =
    []



-- HEADERS


headers : List Css.Snippet
headers =
    []



-- CONTAINERS


containers : List Css.Snippet
containers =
    []


searchResults : List Css.Snippet
searchResults =
    []


pageView : List Css.Snippet
pageView =
    []



-- INPUTS


inputs : List Css.Snippet
inputs =
    [ E.input
        []
    ]



-- MODULARSCALE


config : ModularScale.Config
config =
    { base = [ 1.2 ]
    , interval = ModularScale.MajorSecond
    }


ms : Int -> Em
ms =
    em << ModularScale.get config



-- VARIABLES


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


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


( tiny, small, medium, large, huge ) =
    ( 468, 768, 1024, 1240, 1660 )


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
