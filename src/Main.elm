port module Main exposing (..)

import Date exposing (..)
import Date.Extra exposing (toFormattedString)
import Dict exposing (Dict, insert, member, update)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Html.Lazy as Lazy exposing (lazy, lazy2)
import Http exposing (Error, get, send)
import Icons as Icon
import Json.Decode as Decode exposing (..)
import Navigation exposing (Location)
import Set exposing (Set, insert, member)
import Transit
import Types exposing (..)
import UrlParser as Url exposing ((</>), (<?>), map, oneOf, s, top)


port titlePort : String -> Cmd msg


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
        route =
            parseLocation location

        { query, searchResults, currentPage, cmds } =
            onUrlChange NotAsked route
    in
    { route = route
    , searchQuery = query
    , searchResults = searchResults
    , currentPage = currentPage
    , selectedCategories = Set.empty
    }
        ! cmds


onUrlChange : RemoteData (List Resource) -> Route -> UrlChangeData a Resource
onUrlChange results route =
    let
        getCached id =
            remoteWithDefault [] results
                |> List.filter ((==) id << .id)
                |> List.map Updating
                |> List.head
                |> Maybe.withDefault Loading
    in
    case route of
        Search (Just query) ->
            UrlChangeData query Loading NotAsked [ requestSearchResults query, titlePort query ]

        Page id ->
            UrlChangeData "" NotAsked (getCached id) [ requestPage id ]

        _ ->
            UrlChangeData "" NotAsked NotAsked [ Cmd.none ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            let
                route =
                    parseLocation location

                { searchResults, currentPage, cmds } =
                    onUrlChange model.searchResults route
            in
            { model
                | route = route
                , searchResults = searchResults
                , currentPage = currentPage
            }
                ! cmds

        EnterQuery query ->
            { model | searchQuery = query } ! []

        GotSearchResults (Ok xs) ->
            let
                results =
                    case List.filter ((/=) Nothing << .imageUrl) xs of
                        [] ->
                            xs

                        ys ->
                            List.take 25 ys
            in
            { model | searchResults = Success results, selectedCategories = Set.empty }
                ! []

        GotSearchResults (Err x) ->
            { model | searchResults = Failure x } ! []

        GotPage (Ok xs) ->
            { model | currentPage = Success xs }
                ! [ titlePort (Maybe.withDefault "No-title" (.title xs)) ]

        GotPage (Err x) ->
            { model | currentPage = Failure x } ! []

        SetCategory x ->
            let
                newSelection =
                    if Set.member x model.selectedCategories then
                        Set.remove x model.selectedCategories
                    else
                        Set.insert x model.selectedCategories
            in
            { model | selectedCategories = newSelection } ! []



-- VIEWS


view : Model -> Html Msg
view model =
    let
        activeView =
            case model.route of
                Home ->
                    viewSearch model

                Search _ ->
                    viewSearch model

                Page _ ->
                    Lazy.lazy viewPage model
    in
    main_ []
        [ Lazy.lazy viewHeader model
        , activeView
        ]


viewHeader : Model -> Html Msg
viewHeader { searchQuery, selectedCategories, searchResults } =
    header []
        [ div [ class [ "site-logo" ], onClick (NewUrl "/") ] [ Icon.logo ]
        , Html.form [ onSubmit (NewUrl ("/search/?q=" ++ searchQuery)) ]
            [ input [ onInput EnterQuery, Attr.value searchQuery, placeholder "Search" ] []
            , button [ onClickPreventDefault ("/search/?q=" ++ searchQuery) ] [ Icon.search ]
            ]
        , viewHeaderCategory searchResults selectedCategories
        ]


viewHeaderCategory : RemoteData (List Resource) -> Set String -> Html Msg
viewHeaderCategory results filter =
    let
        resultCategories =
            List.concatMap .category (remoteWithDefault [] results)

        updateCount x xs =
            if Dict.member x xs then
                Dict.update x (Maybe.map ((+) 1)) xs
            else
                Dict.insert x 1 xs

        categoriesWithCount =
            List.foldl updateCount Dict.empty resultCategories
                |> Dict.toList
    in
    ul [ class [ CategoryList ] ] (List.map (categoryItem filter) categoriesWithCount)


categoryItem : Set String -> ( String, Int ) -> Html Msg
categoryItem filter ( cat, count ) =
    li
        [ onClick (SetCategory cat)
        , classList [ ( "selected", Set.member cat filter ) ]
        ]
        [ text (cat ++ " " ++ toString count) ]


viewSearch : Model -> Html Msg
viewSearch model =
    handleViewState model.searchResults (viewSearchList model.selectedCategories)


viewSearchList : Set String -> List Resource -> Html Msg
viewSearchList filter results =
    let
        filteredResults =
            if Set.isEmpty filter then
                results
            else
                List.filter hasCategory results

        hasCategory =
            not << List.isEmpty << List.filter (flip Set.member filter) << .category
    in
    case filteredResults of
        [] ->
            div [ class [ NotificationView ] ] [ text "No results" ]

        xs ->
            section [ class [ SearchView ] ]
                [ ul [] (List.map viewSearchResult xs)
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
viewPageContent { title, id, imageUrl, category, summary, created } =
    section [ class [ PageView ] ]
        [ img [ src (Maybe.withDefault "" imageUrl) ] []
        , h2 [] [ maybeText "No title" title ]
        , viewDate created
        , p [] [ maybeText "" summary ]
        ]


viewDate : Maybe Date -> Html Msg
viewDate date =
    let
        format =
            Date.Extra.toFormattedString "EEEE, d, y 'at' hh:mm"
    in
    case date of
        Nothing ->
            time [] []

        Just x ->
            time [ datetime (format x) ] [ text (format x) ]


handleViewState : RemoteData a -> (a -> Html Msg) -> Html Msg
handleViewState remoteData succesView =
    case remoteData of
        NotAsked ->
            notificationView "Welcome, please enter a search query above"

        Loading ->
            viewLoading

        Failure _ ->
            notificationView "Requested page is currently unavailable."

        Updating x ->
            succesView x

        Success x ->
            succesView x


notificationView : String -> Html Msg
notificationView x =
    section [ class [ NotificationView ] ] [ text x ]


viewLoading : Html Msg
viewLoading =
    section [ class [ LoadingView ] ]
        [ div [ class [ "loader" ] ] [ div [ class [ "spinner" ] ] [] ]
        ]



-- ROUTING


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Search (Url.s "search" <?> Url.stringParam "q")
        , Url.map Page (Url.s "page" </> Url.int)
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


requestPage : Int -> Cmd Msg
requestPage id =
    let
        url =
            "https://www.entoen.nu/api/base/export?id=" ++ toString id
    in
    Http.get url pageDecoder
        |> Http.send GotPage



--JSON DECODERS


searchResultsDecoder : Decode.Decoder (List Resource)
searchResultsDecoder =
    Decode.list searchResultDecoder


searchResultDecoder : Decode.Decoder Resource
searchResultDecoder =
    Decode.map6
        Resource
        (Decode.maybe
            (Decode.oneOf
                [ Decode.at [ "title", "trans", "en" ] Decode.string
                , Decode.at [ "title", "trans", "nl" ] Decode.string
                ]
            )
        )
        (Decode.at [ "id" ] Decode.int)
        (Decode.maybe (Decode.at [ "preview_url" ] Decode.string))
        (Decode.at [ "category" ] (Decode.list Decode.string))
        (Decode.maybe
            (Decode.oneOf
                [ Decode.at [ "summary", "trans", "en" ] Decode.string
                , Decode.at [ "summary", "trans", "nl" ] Decode.string
                ]
            )
        )
        (Decode.succeed Nothing)


pageDecoder : Decode.Decoder Resource
pageDecoder =
    Decode.map6
        Resource
        (Decode.maybe
            (Decode.oneOf
                [ Decode.at [ "rsc", "title", "trans", "en" ] Decode.string
                , Decode.at [ "rsc", "title", "trans", "nl" ] Decode.string
                ]
            )
        )
        (Decode.at [ "id" ] Decode.int)
        (Decode.maybe (Decode.at [ "preview_url" ] Decode.string))
        (Decode.at [ "rsc", "category" ] Decode.string |> Decode.map List.singleton)
        (Decode.maybe
            (Decode.oneOf
                [ Decode.at [ "rsc", "summary", "trans", "en" ] Decode.string
                , Decode.at [ "rsc", "summary", "trans", "nl" ] Decode.string
                ]
            )
        )
        (Decode.at [ "rsc", "created" ] Decode.string
            |> Decode.map (Date.fromString >> Result.toMaybe)
        )



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


remoteWithDefault : a -> RemoteData a -> a
remoteWithDefault default remote =
    case remote of
        Success value ->
            value

        _ ->
            default
