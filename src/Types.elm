module Types exposing (..)

import Date
import Http
import Navigation
import Set


--TYPE ALIASES


type alias Model =
    { route : Route
    , searchQuery : String
    , searchResults : RemoteData (List Resource)
    , currentPage : RemoteData Resource
    , selectedCategories : Set.Set String
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
    , created : Maybe Date.Date
    }



-- UNION TYPES


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
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


type CssClasses
    = PageView
    | SearchView
    | SearchViewResult
    | NotAskedView
    | LoadingView
    | NotificationView
    | CategoryList
    |SiteLogo
