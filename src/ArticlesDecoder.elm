module ArticlesDecoder exposing (articlesDecoder)

import Json.Decode exposing (..)
import Models exposing (..)
import Time


tagDecoder : Decoder Tag
tagDecoder =
    map OtherTag string


articlesDecoder : Decoder (List Article)
articlesDecoder =
    list articleDecoder


articleDecoder : Decoder Article
articleDecoder =
    map6 Article
        (field "title" string)
        (field "tags" (list tagDecoder))
        (field "articleText" string)
        (field "updatedTime" (map Time.millisToPosix int))
        (field "uid" int)
        (field "isDraft" bool)
