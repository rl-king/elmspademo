module Types exposing (..)

import Http exposing (..)
import Navigation exposing (..)
import Window exposing (..)


type alias Model =
    { route : Route
    , searchQuery : String
    , searchResults : List SearchResult
    , currentPage : Resource
    , windowSize : Window.Size
    }


type Msg
    = NoOp
    | UrlChange Location


type Route
    = Home
    | Search (Maybe String)
    | Page String
    | About


type alias SearchResult =
    { title : Maybe String
    , id : Int
    , imageUrl : Maybe String
    , category : List String
    }


type alias Resource =
    { title : Maybe String
    , id : Int
    , imageUrl : Maybe String
    }
