module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


search : Svg a
search =
    svg
        [ class "search-icon", width "34", height "34", viewBox "0 0 34 34" ]
        [ g [ fill "none", stroke "#444", strokeWidth "3" ]
            [ circle [ cx "14", cy "14", r "8" ] []
            , Svg.path [ d "M20,20 L28,28" ] []
            ]
        ]


logo : Svg a
logo =
    svg [ height "59", viewBox "0 0 64 59", width "64" ]
        [ g [ fill "none", fillRule "evenodd" ]
            [ Svg.path [ d "M64 31.6c2.2 18-31.8 27-43.8 27C8.2 58.6 0 52.6 0 41S6.5 2.8 22.2.3C37.4-2.3 62.6 21.5 64 31.6", fill "#222", fillRule "nonzero" ] []
            , Svg.path [ d "M19 42l6.8-24.3", stroke "#7FD13B", strokeWidth "3" ] []
            , Svg.path [ d "M21.3 41.6l23-7", stroke "#5A6378", strokeWidth "3" ] []
            , Svg.path [ d "M43.5 33.5l-18-16", stroke "#60B5CC", strokeWidth "3" ] []
            , Svg.path [ d "M49.8 34.5c0 3.4-2.8 6-6.3 6-3.4 0-6.3-2.6-6.3-6s3-6 6.3-6c3.5 0 6.3 2.6 6.3 6", fill "#60B5CC", fillRule "nonzero" ] []
            , Svg.path [ d "M31.8 18c0 3.5-2.8 6.3-6.3 6.3s-6.3-2.8-6.3-6.2c0-3.2 2.8-6 6.3-6s6.3 2.8 6.3 6", fill "#7FD13B", fillRule "nonzero" ] []
            , Svg.path [ d "M25.3 41.2c0 3.4-2.8 6.2-6.3 6.2s-6.3-2.8-6.3-6.2c0-3.3 2.8-6 6.3-6s6.3 2.7 6.3 6", fill "#5A6378", fillRule "nonzero" ] []
            ]
        ]
