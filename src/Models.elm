module Models exposing (..)

import Html
import Time


type Tag
    = OtherTag String


showTag : Tag -> String
showTag (OtherTag s) =
    s


type alias Article =
    { title : String
    , tags : List Tag
    , articleText : String
    , updatedTime : Time.Posix
    , uid : Int
    , isDraft : Bool
    }
