module HttpChainViaTask exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Events as HE
import Http
import Json.Decode as D exposing (Decoder)
import Task exposing (Task)



{-
   BOOTSTRAP

   elm-live src/HttpChainViaTask.elm -- --debug
-}


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- UTILS


rootUrl : String
rootUrl =
    "https://jsonplaceholder.typicode.com"



-- MODEL / DOMAIN


type alias User =
    { id : Int
    , name : String
    , email : String
    }


usersDecoder : D.Decoder (List User)
usersDecoder =
    D.list <|
        D.map3 User
            (D.field "id" D.int)
            (D.field "name" D.string)
            (D.field "email" D.string)


type alias Post =
    { id : Int
    , userId : Int
    , title : String
    , body : String
    }


postsDecoder : D.Decoder (List Post)
postsDecoder =
    D.list <|
        D.map4 Post
            (D.field "id" D.int)
            (D.field "userId" D.int)
            (D.field "title" D.string)
            (D.field "body" D.string)


type alias UserWithPosts =
    { user : User, posts : List Post }


type Model
    = NotAsked
    | Loading
    | Failed
    | Loaded (List UserWithPosts)


init : () -> ( Model, Cmd msg )
init () =
    ( NotAsked, Cmd.none )



-- UPDATE


type Msg
    = UserWantsData
    | GotData (Result Http.Error (List UserWithPosts))


handleResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleResponse dec res =
    case res of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case D.decodeString dec body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok a ->
                    Ok a


fetchUsers : Task Http.Error (List User)
fetchUsers =
    -- The resolver describes how to deal with the responses produced by a task
    Http.task
        { method = "GET"
        , url = rootUrl ++ "/users"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver (handleResponse usersDecoder)
        }


fetchPosts : User -> Task Http.Error (List Post)
fetchPosts user =
    Http.task
        { method = "GET"
        , url = rootUrl ++ "/posts?userId=" ++ String.fromInt user.id
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver (handleResponse postsDecoder)
        }


fetchSeq : Task Http.Error (List UserWithPosts)
fetchSeq =
    fetchUsers
        |> Task.andThen
            (\users ->
                fetchUsersWithPosts users
            )


fetchUserWithPosts : User -> Task Http.Error UserWithPosts
fetchUserWithPosts user =
    fetchPosts user
        |> Task.map (\posts -> { user = user, posts = posts })


fetchUsersWithPosts : List User -> Task Http.Error (List UserWithPosts)
fetchUsersWithPosts users =
    Task.sequence (List.map fetchUserWithPosts users)


fetchData : Cmd Msg
fetchData =
    {-
       Task.perform: cannot fail
       Task.attempt: CAN fail!
    -}
    Task.attempt GotData fetchSeq


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        UserWantsData ->
            ( Loading, fetchData )

        GotData res ->
            case res of
                Err _ ->
                    ( Failed, Cmd.none )

                Ok lst ->
                    ( Loaded lst, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    H.div []
        [ case model of
            NotAsked ->
                H.div []
                    [ H.p [] [ H.text "Make a request!" ]
                    , H.button [ HE.onClick UserWantsData ] [ H.text "Click me!" ]
                    ]

            Loading ->
                H.p [] [ H.text "Loading" ]

            Failed ->
                H.p [] [ H.text "Failed!" ]

            Loaded usersWithPosts ->
                let
                    toLi uwp =
                        H.li []
                            [ H.text <|
                                uwp.user.name
                                    ++ " ("
                                    ++ String.fromInt (List.length uwp.posts)
                                    ++ ")"
                            ]
                in
                H.div []
                    [ H.text "Got users with posts!"
                    , H.ul [] <|
                        List.map toLi usersWithPosts
                    ]
        ]
