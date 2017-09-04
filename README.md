# Elm sample app [WIP]
A small single page app written in Elm, highlighting how the language helps you create fun, reliable and maintainable apps. 

## Descriptive 
```elm
type alias Model =
    { route : Route
    , searchQuery : String
    , searchResults : RemoteData (List Resource)
    , currentPage : RemoteData Resource
    , selectedCategories : Set.Set String
    }

type alias Resource =
    { title : Maybe String
    , id : Int
    , imageUrl : Maybe String
    , category : List String
    , summary : Maybe String
    , created : Maybe Date.Date
    }
```

## RemoteData type
```elm
type RemoteData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Updating a
    | Success a
```
```elm
handleViewState : RemoteData a -> (a -> Html Msg) -> Html Msg
handleViewState remoteData succesView =
    case remoteData of
        NotAsked ->
            viewNotification "Welcome, please enter a search query above"

        Loading ->
            viewLoading

        Failure _ ->
            viewNotification "Requested page is currently unavailable"

        Updating x ->
            succesView x

        Success x ->
            succesView x
```

## Explicit handling of (inconsistent) data 

```elm
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

maybeOneOf : List (D.Decoder a) -> D.Decoder (Maybe a)
maybeOneOf =
    D.maybe << D.oneOf
```
# Elm Css
## Typed Css classes and variables

```elm
pageView : List Css.Snippet
pageView =
    [ class PageView
        [ maxWidth (ms 18)
        , margin2 zero auto
        , padding (ms 6)
        , backgroundColor (mono W4)
        ]
    ]

viewPageContent : Resource -> Html Msg
viewPageContent { title } =
    section [ class [ PageView ] ] []
```

```elm
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
```


# Talking to Javascript
## Elm
```elm
port titlePort : String -> Cmd msg
```
## Javascript
```javascript
var app = elm.Main.embed(document.getElementById('app'));

app.ports.titlePort.subscribe(x => updateTitle(x));

function updateTitle(x) {
    document.title = x === "" ? "Elm SPA Demo" : x + " | Elm SPA Demo";
}
```

# Setup
## Installation

`npm install`

## Usage

Start webpack dev server

`npm start`

## Build
`npm run build`

## License

MIT
