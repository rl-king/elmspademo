module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Html.Lazy as Lazy
import Http
import Icons as Icon
import Json.Decode as Decode exposing (..)
import Navigation exposing (Location)
import Types exposing (..)
import UrlParser as Url exposing (..)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( cmds, q, s, c ) =
            onUrlChange (parseLocation location)
    in
    { route = parseLocation location
    , searchQuery = q
    , searchResults = s
    , currentPage = c
    , activeCategory = All
    }
        ! [ cmds ]


onUrlChange : Route -> ( Cmd Msg, String, RemoteData e a, RemoteData e a )
onUrlChange route =
    case route of
        Search (Just query) ->
            ( requestSearchResults query, query, Loading, NotAsked )

        Page id ->
            ( requestPage id, "", NotAsked, Loading )

        _ ->
            ( Cmd.none, "", NotAsked, NotAsked )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            let
                ( cmds, q, s, c ) =
                    onUrlChange (parseLocation location)
            in
            { model
                | route = parseLocation location
                , searchResults = s
                , currentPage = c
            }
                ! [ cmds ]

        EnterQuery query ->
            { model | searchQuery = query } ! []

        GotSearchResults (Ok results) ->
            let
                resultsSelection =
                    case List.filter ((/=) Nothing << .imageUrl) results of
                        [] ->
                            results

                        xs ->
                            List.take 15 xs
            in
            { model | searchResults = Success resultsSelection, activeCategory = All } ! []

        GotSearchResults (Err x) ->
            { model | searchResults = Failure x } ! []

        GotPageWithEdges (Ok xs) ->
            { model | currentPage = Success xs } ! []

        GotPageWithEdges (Err x) ->
            { model | currentPage = Failure x } ! []



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
        [ Lazy.lazy viewHeader model
        , activeView
        ]


viewHeader : Model -> Html Msg
viewHeader { searchQuery, activeCategory, searchResults } =
    header []
        [ div [ class [ "site-logo" ], onClick (NewUrl ("/search/?q=" ++ searchQuery)) ] [ Icon.logo ]
        , Html.form [ onSubmit (NewUrl ("/search/?q=" ++ searchQuery)) ]
            [ input [ onInput EnterQuery, Attr.value searchQuery, placeholder "Search" ] []
            , button [ onClick (NewUrl ("/search/?q=" ++ searchQuery)) ] [ Icon.search ]
            ]

        -- , ul [ class [ "categories" ] ] (List.map viewHeaderCategory categories)
        ]


viewHeaderCategory : String -> Html Msg
viewHeaderCategory category =
    li [] [ text category ]


viewSearch : Model -> Html Msg
viewSearch model =
    handleViewState model.searchResults viewSearchList


viewSearchList : List Resource -> Html Msg
viewSearchList list =
    section [ class [ SearchView ] ]
        [ ul [] (List.map viewSearchResult list)
        ]


viewSearchResult : Resource -> Html Msg
viewSearchResult { title, id, imageUrl, category } =
    let
        url =
            "/page/" ++ toString id
    in
    li [ class [ SearchViewResult ] ]
        [ a [ href url, onClickPreventDefault url ]
            [ img [ src (Maybe.withDefault "" imageUrl) ] []
            , h3 [] [ maybeText "No title" title ]
            ]
        ]


viewPage : Model -> Html Msg
viewPage { currentPage } =
    handleViewState currentPage viewPageContent


viewPageContent : Resource -> Html Msg
viewPageContent { title, id, imageUrl, category } =
    section []
        [ img [ src (Maybe.withDefault "" imageUrl) ] []
        , h3 [] [ maybeText "No title" title ]
        , small [] []
        , p [] []
        ]


handleViewState : RemoteData e a -> (a -> Html Msg) -> Html Msg
handleViewState remoteData succesView =
    case remoteData of
        NotAsked ->
            viewNotAsked

        Loading ->
            viewLoading

        Failure _ ->
            viewError

        Success a ->
            succesView a


viewNotAsked : Html Msg
viewNotAsked =
    div [ class [ NotAskedView ] ] [ text "initializing" ]


viewLoading : Html Msg
viewLoading =
    div [ class [ LoadingView ] ] [ text "loading" ]


viewError : Html Msg
viewError =
    div [ class [ ErrorView ] ] [ text "Requested page is currently unavailable." ]



-- ROUTING


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Search (Url.s "search" <?> Url.stringParam "q")
        , Url.map Page (Url.s "page" </> Url.string)
        ]



-- HTTP REQUESTS


requestSearchResults : String -> Cmd Msg
requestSearchResults query =
    let
        url =
            "https://www.entoen.nu/api/search/?limit=200&format=simple&text=" ++ query
    in
    Http.get url searchResultsDecoder
        |> Http.send GotSearchResults


requestPage : String -> Cmd Msg
requestPage id =
    let
        url =
            "https://www.entoen.nu/api/base/export?id=" ++ id
    in
    Http.get url pageDecoder
        |> Http.send GotPageWithEdges



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
        (Decode.at [ "rsc", "category" ] Decode.string |> Decode.map List.singleton)



--HELPER FUNCTIONS


maybeText : String -> Maybe String -> Html Msg
maybeText default maybeValue =
    Html.text <| Maybe.withDefault default maybeValue


parseLocation : Location -> Route
parseLocation =
    Url.parsePath route >> Maybe.withDefault Home


onClickPreventDefault : String -> Attribute Msg
onClickPreventDefault urlPath =
    onWithOptions "click"
        { preventDefault = True
        , stopPropagation = False
        }
        (Decode.succeed <| NewUrl urlPath)


{ class } =
    Html.CssHelpers.withNamespace ""
