module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


search : Svg a
search =
    svg
        [ class "search-icon", width "38", height "38", viewBox "0 0 38 38" ]
        [ g [ fill "none", stroke "#111", strokeWidth "4" ]
            [ circle [ cx "14", cy "14", r "12" ] []
            , Svg.path [ d "M21,21 L32,32" ] []
            ]
        ]
