module Types exposing (..)

import Date exposing (..)
import Http exposing (..)
import Navigation exposing (..)
import Set exposing (..)


--TYPE ALIASES


type alias Model =
    { route : Route
    , searchQuery : String
    , searchResults : RemoteData (List Resource)
    , currentPage : RemoteData Resource
    , selectedCategories : Set String
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
    , created : Maybe Date
    }



-- UNION TYPES


type Msg
    = NewUrl String
    | UrlChange Location
    | EnterQuery String
    | GotSearchResults (Result Http.Error (List Resource))
    | GotPage (Result Http.Error Resource)
    | SetCategory String


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
    | Other


type CssClasses
    = PageView
    | SearchView
    | SearchViewResult
    | NotAskedView
    | LoadingView
    | NotificationView
    | CategoryList
