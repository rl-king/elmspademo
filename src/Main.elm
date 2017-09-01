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
    let
        ( initCmd, initSearchQuery ) =
            initRequests (parseLocation location)
    in
    { route = parseLocation location
    , searchQuery = initSearchQuery
    , searchResults = NotAsked
    , currentPage = NotAsked
    }
        ! [ initCmd ]


initRequests : Route -> ( Cmd Msg, String )
initRequests route =
    case route of
        Search (Just x) ->
            ( requestSearchResults x, x )

        Page x ->
            ( Cmd.none, "" )

        _ ->
            ( Cmd.none, "" )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            { model | route = parseLocation location } ! []

        EnterQuery query ->
            { model | searchQuery = query } ! []

        GotSearchResults (Ok xs) ->
            { model | searchResults = Success xs } ! []

        GotSearchResults (Err x) ->
            { model | searchResults = Failure x } ! []



-- VIEWS


view : Model -> Html Msg
view model =
    let
        activeView =
            case model.route of
                Home ->
                    viewSearch model

                Search x ->
                    viewSearch model

                Page x ->
                    viewPage model
    in
    main_ []
        [ viewHeader model.searchQuery
        , activeView
        ]


viewHeader : String -> Html Msg
viewHeader query =
    header []
        [ Html.form [ onSubmit (NewUrl ("/search/?q=" ++ query)) ]
            [ input [ onInput EnterQuery, Attr.value query, placeholder "Search" ] []
            , Icon.search
            ]
        ]


viewSearch : Model -> Html Msg
viewSearch model =
    let
        results =
            case model.searchResults of
                NotAsked ->
                    div [] []

                Loading ->
                    div [] []

                Failure e ->
                    div [] []

                Success a ->
                    ul [] (List.map viewSearchResult a)
    in
    section [] [ results ]


viewSearchResult : Resource -> Html Msg
viewSearchResult { title, id, imageUrl, category } =
    li [] [ text <| Maybe.withDefault "No title" title ]


viewPage : Model -> Html Msg
viewPage model =
    section []
        [ img [] []
        , h2 [] []
        , small [] []
        , p [] []
        ]



--HELPER FUNCTIONS


parseLocation : Location -> Route
parseLocation =
    Url.parsePath route >> Maybe.withDefault Home



-- ROUTING


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Search <| Url.s "search" <?> Url.stringParam "q"
        , Url.map Page <| Url.s "page" </> Url.string
        ]



-- HTTP REQUESTS


requestSearchResults : String -> Cmd Msg
requestSearchResults query =
    let
        url =
            "https://www.anp-archief.nl/api/search/?format=simple&text=" ++ query
    in
    Http.get url searchResultsDecoder
        |> Http.send GotSearchResults



--JSON DECODERS


searchResultsDecoder : Decode.Decoder (List Resource)
searchResultsDecoder =
    Decode.list searchResultDecoder


searchResultDecoder : Decode.Decoder Resource
searchResultDecoder =
    Decode.map4
        Resource
        (Decode.maybe <|
            Decode.oneOf
                [ Decode.at [ "title", "trans", "en" ] Decode.string
                , Decode.at [ "title", "trans", "nl" ] Decode.string
                ]
        )
        (Decode.at [ "id" ] Decode.int)
        (Decode.maybe <| Decode.at [ "preview_url" ] Decode.string)
        (Decode.at [ "category" ] (Decode.list Decode.string))


pageDecoder : Decode.Decoder Resource
pageDecoder =
    Decode.map4
        Resource
        (Decode.maybe <|
            Decode.oneOf
                [ Decode.at [ "rsc", "title", "trans", "en" ] Decode.string
                , Decode.at [ "rsc", "title", "trans", "nl" ] Decode.string
                ]
        )
        (Decode.at [ "id" ] Decode.int)
        (Decode.maybe <| Decode.at [ "preview_url" ] Decode.string)
        (Decode.at [ "category" ] (Decode.list Decode.string))
