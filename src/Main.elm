module Main exposing (..)
import Html exposing(..)
import Html.Attributes exposing(..)

mainMarginLeft = style "margin-left" "300px"


title : Html ()
title = div [style "font-size" "x-large", mainMarginLeft] [text "ブログ予定地"]

myProfile : Html ()
myProfile = div [
    style "height" "100%"
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
  [
    div [] [text "profile"],
    ul [] [
      li [] [text "なまえ: tsukimizake"],
      li [] [text "趣味: 柔術"],
      li [] [text "将来の夢: 農家"]  
    ]
  ]

main : Html ()
main = div [] [myProfile, title, div [style "margin-top" "60px", mainMarginLeft] [ text "ほにほに！ふわっ！ふわっ！ ٩(*╹ω╹*)و！！"]]
