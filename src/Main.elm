port module Main exposing (..)

import Date exposing (..)
import Date.Extra exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Html.Lazy as Lazy exposing (lazy, lazy2)
import Http exposing (Error, get, send)
import Icons as Icon
import Json.Decode as Decode exposing (..)
import Navigation exposing (Location)
import Types exposing (..)
import UrlParser as Url exposing ((</>), (<?>), map, oneOf, s, top)


port htmlTitle : String -> Cmd msg


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
    , activeCategory = All
    }
        ! cmds


onUrlChange : RemoteData (List Resource) -> Route -> UrlChangeData a Resource
onUrlChange results route =
    let
        checkCache id =
            case results of
                Success xs ->
                    List.filter ((==) id << .id) xs
                        |> List.head
                        |> Maybe.map Updating
                        |> Maybe.withDefault Loading

                _ ->
                    Loading
    in
    case route of
        Search (Just query) ->
            UrlChangeData query Loading NotAsked [ requestSearchResults query, htmlTitle query ]

        Page id ->
            UrlChangeData "" NotAsked (checkCache id) [ requestPage id ]

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
                            List.take 15 ys
            in
            { model | searchResults = Success results, activeCategory = All }
                ! []

        GotSearchResults (Err x) ->
            { model | searchResults = Failure x } ! []

        GotPageWithEdges (Ok xs) ->
            { model | currentPage = Success xs }
                ! [ htmlTitle (Maybe.withDefault "No-title" (.title xs)) ]

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
viewHeader { searchQuery, activeCategory, searchResults } =
    header []
        [ div [ class [ "site-logo" ], onClick (NewUrl "/") ] [ Icon.logo ]
        , Html.form [ onSubmit (NewUrl ("/search/?q=" ++ searchQuery)) ]
            [ input [ onInput EnterQuery, Attr.value searchQuery, placeholder "Search" ] []
            , button [ onClickPreventDefault ("/search/?q=" ++ searchQuery) ] [ Icon.search ]
            ]
        , ul [ class [ CategoryList ] ]
            (List.map viewHeaderCategory
                [ Product, Media, Image, WindowImage, Article, Text, All ]
            )
        ]


viewHeaderCategory : Category -> Html Msg
viewHeaderCategory category =
    li [] [ text (categoryToString category) ]


viewSearch : Model -> Html Msg
viewSearch model =
    handleViewState model.searchResults viewSearchList


viewSearchList : List Resource -> Html Msg
viewSearchList list =
    case list of
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
        , time [] [ text (viewDate created) ]
        , p [] [ maybeText "" summary ]
        ]


viewDate : Maybe Date -> String
viewDate date =
    case date of
        Nothing ->
            ""

        Just x ->
            Date.Extra.toFormattedString "EEEE, MMMM d, y 'at' h:mm a" x


handleViewState : RemoteData a -> (a -> Html Msg) -> Html Msg
handleViewState remoteData succesView =
    case remoteData of
        NotAsked ->
            notificationView "Welcome, please enter a search query above"

        Loading ->
            viewLoading

        Failure _ ->
            notificationView "Requested page is currently unavailable."

        Updating a ->
            succesView a

        Success a ->
            succesView a


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
        |> Http.send GotPageWithEdges



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
        (Decode.at [ "category" ] (Decode.list Decode.string)
            |> Decode.map toCategoryType
        )
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
        (Decode.at [ "rsc", "category" ] Decode.string
            |> Decode.map (toCategoryType << List.singleton)
        )
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


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


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


toCategoryType : List String -> List Category
toCategoryType =
    List.map stringToCategory


stringToCategory : String -> Category
stringToCategory x =
    case String.toLower x of
        "product" ->
            Product

        "media" ->
            Media

        "image" ->
            Image

        "windowimage" ->
            WindowImage

        "article" ->
            Article

        "text" ->
            Text

        _ ->
            All


categoryToString : Category -> String
categoryToString x =
    case x of
        Product ->
            "Product"

        Media ->
            "Media"

        Image ->
            "Image"

        WindowImage ->
            "Windowimage"

        Article ->
            "Article"

        Text ->
            "Text"

        All ->
            "All"
