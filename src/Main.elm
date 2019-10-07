module Main exposing (Model(..), Msg(..), Post, getPosts, init, main, postDecoder, postListDecoder, subscriptions, update, view, viewPost, viewPosts)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, map4, map5, string)
import Time exposing (millisToPosix, toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (List Post)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getPosts )



-- UPDATE


type alias Category =
    { id : Int
    , name : String
    , permalink : String
    }


type alias Post =
    { id : Int
    , title : String
    , timestamp : Int
    , permalink : String
    , categories : List Category
    }


type Msg
    = MorePlease
    | GotPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getPosts )

        GotPosts result ->
            case result of
                Ok posts ->
                    ( Success posts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW HELPERS


toUtcString : Time.Posix -> String
toUtcString timestamp =
    let
        dateTimeString =
            String.fromInt (toYear utc timestamp)
    in
        dateTimeString



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Posts" ]
        , viewPosts model
        ]


viewPostDate : Post -> Html Msg
viewPostDate post =
    let
        posixTimestamp =
            millisToPosix post.timestamp
    in
    p [] [ text (toUtcString posixTimestamp) ]


viewPost : Post -> Html Msg
viewPost post =
    div []
        [ h4 []
            [ a [ href post.permalink, target "_blank" ] [ text post.title ], viewPostDate post ]
        , div [] (List.map viewCategory post.categories)
        ]


viewCategory : Category -> Html Msg
viewCategory category =
    div [] [ text category.name ]


viewPosts : Model -> Html Msg
viewPosts model =
    case model of
        Failure ->
            div []
                [ text "Failed to load posts."
                ]

        Loading ->
            text "Loading..."

        Success posts ->
            div [] (List.map viewPost posts)



-- HTTP


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "https://ylva.fi/wp-json/swiss/v1/feed?lang=fi"
        , expect = Http.expectJson GotPosts postListDecoder
        }


postDecoder =
    map5 Post
        (field "id" int)
        (field "title" string)
        (field "timestamp" int)
        (field "permalink" string)
        (field "categories" (list categoryDecoder))


categoryDecoder =
    map3 Category
        (field "id" int)
        (field "name" string)
        (field "permalink" string)


postListDecoder : Decoder (List Post)
postListDecoder =
    list postDecoder
