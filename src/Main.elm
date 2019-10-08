module Main exposing (Model(..), Msg(..), Post, getPosts, init, main, postDecoder, postListDecoder, subscriptions, update, view, viewPost, viewPosts)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, map4, map5, map6, maybe, string)



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
    , featuredImage : Maybe String
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



-- HELPERS
catsFromPosts : List Post -> List Category
catsFromPosts posts =
    posts
        |> List.map .categories
        |> List.concat

{-
   Given a List of Posts and a Category, return a filtered List of Posts where
   only members of Category are included.
-}

filterPostsByCategory : List Post -> Category -> List Post
filterPostsByCategory posts category =
    List.filter (\p -> List.member category p.categories) posts



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Posts" ]
        , viewFilters model
        , viewPosts model
        ]


viewPostImage : Post -> Html Msg
viewPostImage post =
    case post.featuredImage of
        Nothing ->
            text "So you gave me a post with no image"

        Just val ->
            img [ src val ] []


viewPost : Post -> Html Msg
viewPost post =
    div []
        [ h4 []
            [ a [ href post.permalink, target "_blank" ] [ text post.title ] ]
        , div [] (List.map viewCategory post.categories)
        , viewPostImage post
        ]


viewCategory : Category -> Html Msg
viewCategory category =
    div [] [ text category.name ]


viewFilters : Model -> Html Msg
viewFilters model =
    fieldset []
        [ label [] [ input [ type_ "checkbox", value "rofl1" ] [], text "Lol1" ]
        , label [] [ input [ type_ "checkbox", value "rofl2" ] [], text "Lol2" ]
        ]


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
            div [] (List.map viewPost (filterPostsByCategory posts { id = 15, name = "Majoitus", permalink = "https://ylva.fi/category/majoitus/" }))



-- HTTP


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "https://ylva.fi/wp-json/swiss/v1/feed?lang=fi"
        , expect = Http.expectJson GotPosts postListDecoder
        }



-- JSON


postDecoder =
    map6 Post
        (field "id" int)
        (field "title" string)
        (field "timestamp" int)
        (field "permalink" string)
        (maybe (field "featuredImage" string))
        (field "categories" (list categoryDecoder))


categoryDecoder =
    map3 Category
        (field "id" int)
        (field "name" string)
        (field "permalink" string)


postListDecoder : Decoder (List Post)
postListDecoder =
    list postDecoder
