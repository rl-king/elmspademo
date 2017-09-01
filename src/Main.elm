module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy as Lazy
import Http
import Icons as Icon
import Json.Decode as Decode exposing (..)
import Navigation exposing (Location)
import Task
import Types exposing (..)
import UrlParser as Url exposing (..)
import Window


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { route = parseLocation location
    , searchQuery = Maybe.withDefault "" Nothing
    , searchResults = []
    , currentPage = Resource Nothing 0 Nothing
    , windowSize = Window.Size 0 0
    }
        ! []


view : Model -> Html Msg
view model =
    main_ [] []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            model ! []



--HELPER FUNCTIONS


parseLocation : Location -> Route
parseLocation location =
    Maybe.withDefault Home (Url.parsePath route location)



-- ROUTING


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Search <| Url.s "search" <?> Url.stringParam "q"
        , Url.map Page <| Url.s "page" </> Url.string
        , Url.map About <| Url.s "about"
        ]



--JSON DECODERS


searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    Decode.list <|
        Decode.map4 SearchResult
            (Decode.maybe <|
                Decode.oneOf
                    [ Decode.at [ "title", "trans", "en" ] Decode.string
                    , Decode.at [ "title", "trans", "nl" ] Decode.string
                    ]
            )
            (Decode.at [ "id" ] Decode.int)
            (Decode.maybe <| Decode.at [ "preview_url" ] Decode.string)
            (Decode.at [ "category" ] (Decode.list Decode.string))


resourceDecoder : Decode.Decoder Resource
resourceDecoder =
    Decode.map3
        Resource
        (Decode.maybe <|
            Decode.oneOf
                [ Decode.at [ "rsc", "title", "trans", "en" ] Decode.string
                , Decode.at [ "rsc", "title", "trans", "nl" ] Decode.string
                ]
        )
        (Decode.at [ "id" ] Decode.int)
        (Decode.maybe <| Decode.at [ "preview_url" ] Decode.string)
