module Models exposing (..)

import Html
import Time


type Tag
    = OtherTag String


showTag : Tag -> Html.Html msg
showTag (OtherTag s) =
    Html.div [] [ Html.text s ]


type alias Article =
    { title : String
    , tags : List Tag
    , articleText : String
    , updatedTime : Time.Posix
    }
