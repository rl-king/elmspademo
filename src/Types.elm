module Types exposing (..)

import Http exposing (..)
import Navigation exposing (..)


--TYPE ALIASES


type alias Model =
    { route : Route
    , searchQuery : String
    , searchResults : RemoteData Http.Error (List Resource)
    , currentPage : RemoteData Http.Error Resource
    }


type alias Resource =
    { title : Maybe String
    , id : Int
    , imageUrl : Maybe String
    , category : List String
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
    | Page String


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type CssClasses
    = PageView
    | SearchView
    | SearchViewResult
