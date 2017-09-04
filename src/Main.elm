port module Main exposing (..)

import Animation exposing (px)
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
import Json.Decode as D
import Navigation exposing (Location)
import Set exposing (Set, insert, member)
import Types exposing (..)
import UrlParser as Url exposing ((</>), (<?>), map, oneOf, s, top)


port titlePort : String -> Cmd msg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.animation ]


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
    , animation = pageAnimation
    }
        ! cmds


pageAnimation : Animation.State
pageAnimation =
    Animation.interrupt
        [ Animation.set
            [ Animation.opacity 0
            , Animation.translate (px 0) (px 100)
            ]
        , Animation.toWithEach
            [ ( Animation.spring
                    { stiffness = 400
                    , damping = 50
                    }
              , Animation.opacity 1
              )
            , ( Animation.spring
                    { stiffness = 80
                    , damping = 8
                    }
              , Animation.translate (px 0) (px 0)
              )
            ]
        ]
        (Animation.style
            [ Animation.opacity 0
            ]
        )


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
                , animation = pageAnimation
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
            { model
                | searchResults = Success results
                , selectedCategories = Set.empty
            }
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

        Animate animMsg ->
            { model | animation = Animation.update animMsg model.animation } ! []



-- VIEWS


view : Model -> Html Msg
view model =
    let
        activeView =
            case model.route of
                Home ->
                    viewSearch model

                Search _ ->
                    Lazy.lazy viewSearch model

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
        [ div [ class [ SiteLogo ], onClick (NewUrl "/") ] [ Icon.logo ]
        , Html.form [ onSubmit (NewUrl ("/search/?q=" ++ searchQuery)) ]
            [ input
                [ autofocus True
                , onInput EnterQuery
                , Attr.value searchQuery
                , placeholder "Search"
                ]
                []
            , button [] [ Icon.search ]
            ]
        , viewCategory searchResults selectedCategories
        ]


viewCategory : RemoteData (List Resource) -> Set String -> Html Msg
viewCategory results filter =
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
    ul [ class [ CategoryList ] ] (List.map (viewCategoryItem filter) categoriesWithCount)


viewCategoryItem : Set String -> ( String, Int ) -> Html Msg
viewCategoryItem filter ( cat, count ) =
    li
        [ onClick (SetCategory cat)
        , classList [ ( "selected", Set.member cat filter ) ]
        ]
        [ text (cat ++ " " ++ toString count) ]


viewSearch : Model -> Html Msg
viewSearch { searchResults, selectedCategories } =
    handleViewState searchResults (viewSearchList selectedCategories)


viewSearchList : Set String -> List Resource -> Html Msg
viewSearchList filter results =
    let
        hasCategory =
            not << List.isEmpty << List.filter (flip Set.member filter) << .category

        filteredResults =
            if Set.isEmpty filter then
                results
            else
                List.filter hasCategory results
    in
    case filteredResults of
        [] ->
            viewNotification "No results"

        xs ->
            section [ class [ SearchView ] ]
                [ ul [] (List.map viewSearchItem xs)
                ]


viewSearchItem : Resource -> Html Msg
viewSearchItem { title, id, imageUrl, category } =
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
viewPage { currentPage, animation } =
    handleViewState currentPage (viewPageContent animation)


viewPageContent : Animation.State -> Resource -> Html Msg
viewPageContent animation { title, id, imageUrl, category, summary, created } =
    section
        (Animation.render animation
            ++ [ class [ PageView ] ]
        )
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
handleViewState remoteData successView =
    case remoteData of
        NotAsked ->
            viewNotification "Welcome, please enter a search query above"

        Loading ->
            viewLoading

        Failure _ ->
            viewNotification "Requested page is currently unavailable"

        Updating x ->
            successView x

        Success x ->
            successView x


viewNotification : String -> Html Msg
viewNotification x =
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


maybeOneOf : List (D.Decoder a) -> D.Decoder (Maybe a)
maybeOneOf =
    D.maybe << D.oneOf


searchResultsDecoder : D.Decoder (List Resource)
searchResultsDecoder =
    D.list searchResultDecoder


searchResultDecoder : D.Decoder Resource
searchResultDecoder =
    D.map6
        Resource
        (maybeOneOf
            [ D.at [ "title", "trans", "en" ] D.string
            , D.at [ "title", "trans", "nl" ] D.string
            ]
        )
        (D.field "id" D.int)
        (D.field "preview_url" D.string |> D.maybe)
        (D.field "category" (D.list D.string))
        (maybeOneOf
            [ D.at [ "summary", "trans", "en" ] D.string
            , D.at [ "summary", "trans", "nl" ] D.string
            ]
        )
        (D.succeed Nothing)


pageDecoder : D.Decoder Resource
pageDecoder =
    D.map6
        Resource
        (maybeOneOf
            [ D.at [ "rsc", "title", "trans", "en" ] D.string
            , D.at [ "rsc", "title", "trans", "nl" ] D.string
            ]
        )
        (D.field "id" D.int)
        (D.field "preview_url" D.string |> D.maybe)
        (D.at [ "rsc", "category" ] D.string |> D.map List.singleton)
        (maybeOneOf
            [ D.at [ "rsc", "summary", "trans", "en" ] D.string
            , D.at [ "rsc", "summary", "trans", "nl" ] D.string
            ]
        )
        (D.at [ "rsc", "created" ] D.string
            |> D.map (Date.fromString >> Result.toMaybe)
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
        (D.succeed <| NewUrl urlPath)


{ class } =
    Html.CssHelpers.withNamespace ""


remoteWithDefault : a -> RemoteData a -> a
remoteWithDefault default remote =
    case remote of
        Success value ->
            value

        _ ->
            default
