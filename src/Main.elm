module Main exposing (Model(..), Msg(..), Post, getPosts, init, main, postDecoder, postListDecoder, subscriptions, update, view, viewPost, viewPosts)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, map4, string)



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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Posts" ]
        , viewPosts model
        ]


viewPost : Post -> Html Msg
viewPost post =
    div []
        [ h4 []
            [ a [ href post.permalink, target "_blank" ] [ text post.title ]
            ]
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
                , button [ onClick MorePlease ] [ text "Try Again!" ]
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
    map4 Post
        (field "id" int)
        (field "title" string)
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
