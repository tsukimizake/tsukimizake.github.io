module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time


mainMarginLeft : Attribute ()
mainMarginLeft =
    style "margin-left" "300px"


type Tag
    = Tag String


type BlogPost
    = BlogPost
        { title : String
        , tags : List Tag
        , article : String
        , updatedTime : Time.Posix
        }


blogPosts : List BlogPost
blogPosts =
    [ BlogPost { title = "初記事", tags = [ Tag "system" ], article = "ほにほに！ふわっ！ふわっ！ ٩(*╹ω╹*)و！！", updatedTime = Time.millisToPosix 158771117000 } ]


blogPostsView : List BlogPost -> Html ()
blogPostsView _ =
    div [] []


title : Html ()
title =
    div [ style "font-size" "x-large" ] [ text "ブログ予定地" ]


myProfile : Html ()
myProfile =
    div
        [ style "height" "100%"
        , style "background-color" "#E0E0E0"
        , style "position" "fixed"
        , style "z-index" "1"
        , style "left" "0"
        , style "top" "0"
        , style "overflow-x" "hidden"
        , style "padding-top" "60px"
        , style "padding-right" "60px"
        , style "padding-left" "30px"
        , style "transition" "0.5s"
        ]
        [ div [] [ text "profile" ]
        , ul []
            [ li [] [ text "なまえ: tsukimizake" ]
            , li [] [ text "最近の趣味: 柔術" ]
            , li [] [ text "将来の夢: 農家" ]
            ]
        ]


main : Html ()
main =
    div [] [ myProfile, div [ mainMarginLeft ] [ title, blogPostsView blogPosts ] ]
