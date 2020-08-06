port module Main exposing (main)

import ArticlesDecoder exposing (articlesDecoder)
import Browser exposing (..)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import List
import Markdown
import Models exposing (..)
import Time
import Url as Url
import Url.Parser as UP exposing ((<?>))
import Url.Parser.Query as UQ


type Msg
    = GotArticles (Result Error (List Article))
    | Nop
    | UrlChanged Url.Url
    | LinkClicked UrlRequest



--TODO debugMode変更し忘れてcommitすると記事表示されないので何かマシな方法を


debugMode : Bool
debugMode =
    False


type alias Model =
    { articles : List Article
    , url : Url.Url
    , key : Nav.Key
    }


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


showDate : Time.Zone -> Time.Posix -> String
showDate zone time =
    String.fromInt (Time.toYear zone time)
        ++ "/"
        ++ (String.fromInt <|
                toIntMonth <|
                    Time.toMonth zone time
           )
        ++ "/"
        ++ String.fromInt (Time.toDay zone time)


articleView : Time.Zone -> Article -> Html Msg
articleView zone post =
    let
        tags : List String
        tags =
            List.map showTag post.tags

        tagsView : List (Html Msg)
        tagsView =
            List.map (\s -> div [ class "tag" ] [ text s ]) tags
    in
    div [ class "article" ]
        [ ul []
            [ h1 [] [ text post.title ]
            , div [] [ text <| "投稿日:" ++ showTime zone post.updatedTime ]
            , ul [] [ li [ style "list-style" "none" ] (text "タグ: " :: tagsView) ]
            , div [ class "articleText" ] [ Markdown.toHtml [] post.articleText ]
            ]
        ]


articlesView : Time.Zone -> Url.Url -> List Article -> Html Msg
articlesView zone url articles =
    let
        route =
            UP.parse routeParser url
    in
    case route of
        Just (BlogPost (Just n)) ->
            let
                article =
                    List.filter (\x -> x.uid == n) articles
            in
            ul [] <| List.map (articleView zone) article

        Nothing ->
            ul [] <| List.map (articleView zone) articles

        Just (BlogPost Nothing) ->
            ul [] <| List.map (articleView zone) articles

        Just Profile ->
            ul [] <| List.map (articleView zone) articles


pageTitle : Html msg
pageTitle =
    div [ class "pagetitle" ] [ text "我々はどこから来たのか、我々は何者か、我々はなぜもふもふでないのか" ]


myProfile : Html msg
myProfile =
    div []
        [ text "書いてる人"
        , ul
            []
            [ li [] [ text "なまえ: tsukimizake" ]
            , li [] [ text "最近の趣味: 柔術" ]
            , li [] [ text "将来の夢: 農家" ]
            ]
        ]


articleList : Model -> Html msg
articleList model =
    div []
        [ text "記事一覧"
        , ul [] (List.map (\article -> li [] [ a [ href <| "?post=" ++ String.fromInt article.uid ] [ text <| article.title ++ " " ++ showDate jst article.updatedTime ] ]) model.articles)
        ]


leftMenu : Model -> Html msg
leftMenu model =
    div
        [ class "leftMenu" ]
        [ myProfile, articleList model ]


jst : Time.Zone
jst =
    Time.customZone (9 * 60)
        []


articlesUrl : String
articlesUrl =
    if debugMode then
        -- serve with http-server --cors='*' .
        "http://127.0.0.1:8080/articles.json"

    else
        "https://tsukimizake.github.io/articles.json"


view : Model -> Document Msg
view model =
    { title = "ブログ予定地"
    , body =
        [ div [ class "rootWrapper" ]
            [ leftMenu model
            , div [] [ pageTitle, articlesView jst model.url model.articles ]
            ]
        ]
    }


type Route
    = BlogPost (Maybe Int)
    | Profile


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map BlogPost (UP.top <?> UQ.int "post")
        , UP.map Profile <| UP.s "profile"
        ]


port callKatex : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotArticles result ->
            case result of
                Ok articles ->
                    ( { model | articles = List.reverse <| List.sortBy (\article -> article.uid) articles }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Nop ->
            ( model, Cmd.none )

        UrlChanged url ->
            Debug.log "uc" ( { model | url = url }, callKatex () )

        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { articles = [], url = url, key = key }, Cmd.batch [ Http.get { url = articlesUrl, expect = expectJson GotArticles articlesDecoder } ] )


main : Platform.Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
