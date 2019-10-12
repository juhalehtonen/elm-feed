module Main exposing (Category, Model, Msg(..), Post, Status(..), categoryDecoder, catsFromPosts, filterPostsByCategory, getPosts, init, main, postDecoder, postListDecoder, subscriptions, update, view, viewCategory, viewFilter, viewFilters, viewPost, viewPostImage, viewPosts)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, map4, map5, map6, maybe, string)
import List.Extra exposing (find, unique, uniqueBy)



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


type alias Model =
    { posts : List Post
    , filteredPosts : List Post
    , status : Status
    , categories : List Category
    , filteredCategories : List Category
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = [], filteredPosts = [], categories = [], filteredCategories = [], status = Loading }, getPosts )



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
    | FilterPosts Category Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilterPosts category checked ->
            ( { model | filteredPosts = producePostsToShow model category checked, filteredCategories = produceCategoriesToFilter model category checked }, Cmd.none )

        GotPosts result ->
            case result of
                Ok posts ->
                    ( { model | status = Success, posts = posts, filteredPosts = posts, categories = catsFromPosts posts }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HELPERS
{-
   Handling for categories that are filters handled here.
   TODO : Try to make it so that we don't have to calculate this stuff twice to get both cats and posts
-}


produceCategoriesToFilter : Model -> Category -> Bool -> List Category
produceCategoriesToFilter model category filterChecked =
    case filterChecked of
        True ->
            category :: model.filteredCategories

        False ->
            List.Extra.remove category model.filteredCategories



{-
   The main filtering logic for posts resides here.
-}


producePostsToShow : Model -> Category -> Bool -> List Post
producePostsToShow model category filterChecked =
    case filterChecked of
        -- Filter was checked so a new filter was added in place
        True ->
            filterPostsByCategories model.posts (category :: model.filteredCategories)

        False ->
            if List.length model.filteredCategories == 1 then
                -- If we have nothing selected, then return everything
                -- This is a bit stupid ATM, but we are essentially checking if there was only
                -- 1 item in the model. If that gets removed, nothing remains.
                model.posts

            else
                -- If we have at least something selected, filter
                filterPostsByCategories model.posts (List.Extra.remove category model.filteredCategories)



{-
   Given a List of Posts, extract a List of unique Categories based on their id.
   This is typically only run once, at the initialization of this app in order to
   create a list of Categories to filter content by.
-}


catsFromPosts : List Post -> List Category
catsFromPosts posts =
    posts
        |> List.map .categories
        |> List.concat
        |> uniqueBy (\p -> p.id)



{-
   Given a Post and a List of Categories, determine if the Post belongs
   to any of the Categories.
-}


isPostInCats : Post -> List Category -> Bool
isPostInCats post categories =
    post.categories
        |> List.any (\c -> List.member c categories)



{-
   Given a List of Posts and a Category, return a filtered List of Posts where
   only members of Category are included.
-}


filterPostsByCategory : List Post -> Category -> List Post
filterPostsByCategory posts category =
    List.filter (\p -> List.member category p.categories) posts



{-
   Given a List of Posts and a List of Categories, return a List of Posts
   that belong to any of the said Categories.
-}


filterPostsByCategories : List Post -> List Category -> List Post
filterPostsByCategories posts categories =
    posts
        |> List.filter (\post -> isPostInCats post categories)



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
            p [] [ text "post with no image" ]

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
    label [] [ input [ type_ "checkbox", onCheck (FilterPosts category) ] [], text category.name ]


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
                , div [] (List.map viewPost model.filteredPosts)
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
