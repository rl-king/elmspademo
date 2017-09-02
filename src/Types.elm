module Types exposing (..)

import Http exposing (..)
import Navigation exposing (..)


--TYPE ALIASES


type alias Model =
    { route : Route
    , searchQuery : String
    , searchResults : RemoteData (List Resource)
    , currentPage : RemoteData Resource
    , activeCategory : Category
    }


type alias UrlChangeData a b =
    { query : String
    , searchResults : RemoteData a
    , currentPage : RemoteData b
    , cmds : List (Cmd Msg)
    }


type alias Resource =
    { title : Maybe String
    , id : Int
    , imageUrl : Maybe String
    , category : List String
    , summary : Maybe String
    }



-- UNION TYPES


type Msg
    = NewUrl String
    | UrlChange Location
    | EnterQuery String
    | GotSearchResults (Result Http.Error (List Resource))
    | GotPageWithEdges (Result Http.Error Resource)


type Route
    = Home
    | Search (Maybe String)
    | Page Int


type RemoteData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Updating a
    | Success a


type Category
    = Product
    | Media
    | Image
    | Text
    | WindowImage
    | Article
    | All


type CssClasses
    = PageView
    | SearchView
    | SearchViewResult
    | NotAskedView
    | LoadingView
    | ErrorView
