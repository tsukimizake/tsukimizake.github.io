-- TODO
-- markdown読み込み
-- math-markdownでの数学記号
-- 記事検索


module Main exposing (main)

import ArticlesDecoder exposing (articlesDecoder)
import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (style)
import Http exposing (..)
import List
import Markdown
import Models exposing (..)
import Time


type Msg
    = GotArticles (Result Error (List Article))
    | NoOp


debugMode : Bool
debugMode =
    True


type alias Model =
    { articles : List Article }


mainMarginLeft : Attribute msg
mainMarginLeft =
    style "margin-left" "300px"


toIntMonth : Time.Month -> Int
toIntMonth month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


showTime : Time.Zone -> Time.Posix -> String
showTime zone time =
    String.fromInt (Time.toYear zone time)
        ++ "年 "
        ++ (String.fromInt <|
                toIntMonth <|
                    Time.toMonth zone time
           )
        ++ "月"
        ++ String.fromInt (Time.toDay zone time)
        ++ "日 "
        ++ String.fromInt (Time.toHour zone time)
        ++ ":"
        ++ String.fromInt (Time.toMinute zone time)
        ++ ":"
        ++ String.fromInt (Time.toSecond zone time)


articleView : Time.Zone -> Article -> Html Msg
articleView zone post =
    div []
        [ ul []
            [ div [ style "font-size" "large" ] [ text post.title ]
            , div [] [ text <| "投稿日:" ++ showTime zone post.updatedTime ]
            , ul [] [ li [ style "list-style" "none" ] (text "タグ: " :: List.map (text << showTag) post.tags) ]
            , div [] [ Markdown.toHtml [] post.articleText ]
            ]
        ]


articlesView : Time.Zone -> List Article -> Html Msg
articlesView zone articles =
    ul [] <| List.map (articleView zone) articles


pageTitle : Html msg
pageTitle =
    div [ style "font-size" "x-large" ] [ text "ブログ予定地" ]


myProfile : Html msg
myProfile =
    div
        [ style "height" "100%"
        , style "background-color" "#F0F0F0"
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


jst : Time.Zone
jst =
    Time.customZone (9 * 60)
        []


articlesUrl : String
articlesUrl =
    if debugMode then
        "./articles.json"

    else
        "https://tsukimizake.github.io/articles.json"


view : Model -> Document Msg
view model =
    { title = "ブログ予定地", body = [ div [] [ myProfile, div [ mainMarginLeft ] [ pageTitle, articlesView jst model.articles ] ] ] }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotArticles result ->
            case result of
                Ok articles ->
                    ( { model | articles = articles }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Platform.Program () Model Msg
main =
    document
        { init = \_ -> ( { articles = [] }, Http.get { url = articlesUrl, expect = expectJson GotArticles articlesDecoder } )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
