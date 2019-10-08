import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, map4, map5, map6, maybe, string)
import List.Extra exposing (unique, uniqueBy)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Status
    = Failure
    | Loading
    | Success


type alias Model
    = { posts: List Post
      , filteredPosts: List Post
      , status: Status
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( {posts=[], filteredPosts=[], status=Loading}, getPosts )



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
    = GotPosts (Result Http.Error (List Post))
    | FilterPosts Category


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilterPosts category ->
            ( {model | filteredPosts=(filterPostsByCategory model.posts category)}, Cmd.none )

        GotPosts result ->
            case result of
                Ok posts ->
                    ( {status=Success, posts=posts, filteredPosts=posts}, Cmd.none )

                Err _ ->
                    ( {model | status=Failure}, Cmd.none )



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
        |> uniqueBy (\p -> p.id)



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




viewFilter : Category -> Html Msg
viewFilter category =
    label [] [ input [ type_ "checkbox", value (String.fromInt category.id) ] [], text category.name ]


viewFilters : List Post -> Html Msg
viewFilters posts =
    fieldset [] (List.map viewFilter (catsFromPosts posts))


viewPosts : Model -> Html Msg
viewPosts model =
    case model.status of
        Failure ->
            div []
                [ text "Failed to load posts."
                ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ div [] [ viewFilters model.posts ]
                , div [] (List.map viewPost (filterPostsByCategory model.filteredPosts { id = 15, name = "Majoitus", permalink = "https://ylva.fi/category/majoitus/" }))
                ]



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
