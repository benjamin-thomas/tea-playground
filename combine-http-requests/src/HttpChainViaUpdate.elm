module HttpChainViaUpdate exposing (main)

{-
   elm-live src/HttpChainViaUpdate.elm -- --debug
-}

import Browser
import Html as H exposing (Html)
import Html.Events as HE
import Http
import Json.Decode as D


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
    { user : User
    , posts : List Post
    }


type Model
    = NotAsked
    | LoadingUsers
    | LoadingPosts (List User)
    | Failed
    | Loaded (List UserWithPosts)


init : () -> ( Model, Cmd msg )
init () =
    ( NotAsked, Cmd.none )


type Msg
    = GetData
    | GotUsers (Result Http.Error (List User))
    | GotPosts (Result Http.Error (List Post))


fetchUsers : Cmd Msg
fetchUsers =
    Http.get
        { url = rootUrl ++ "/users"
        , expect = Http.expectJson GotUsers usersDecoder
        }


fetchUserPosts : User -> Cmd Msg
fetchUserPosts user =
    Http.get
        { url = rootUrl ++ "/posts?userId=" ++ String.fromInt user.id
        , expect = Http.expectJson GotPosts postsDecoder
        }


fetchPosts : List User -> Cmd Msg
fetchPosts users =
    Cmd.batch <|
        List.map fetchUserPosts users


buildUserWithPosts : List User -> List Post -> List UserWithPosts
buildUserWithPosts users posts =
    let
        userWithPosts : User -> UserWithPosts
        userWithPosts user =
            { user = user, posts = posts }
    in
    List.map userWithPosts users


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, GetData ) ->
            ( LoadingUsers, fetchUsers )

        ( LoadingUsers, GotUsers res ) ->
            case res of
                Err _ ->
                    ( Failed, Cmd.none )

                Ok users ->
                    ( LoadingPosts users, fetchPosts users )

        ( LoadingPosts users, GotPosts res ) ->
            case res of
                Err _ ->
                    ( Failed, Cmd.none )

                Ok posts ->
                    ( Loaded <| buildUserWithPosts users posts, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


rootUrl =
    "https://jsonplaceholder.typicode.com"


view : Model -> Html Msg
view model =
    H.div [] <|
        case model of
            NotAsked ->
                [ H.p [] [ H.text "Make a request!" ]
                , H.button [ HE.onClick GetData ] [ H.text "Click me!" ]
                ]

            LoadingUsers ->
                [ H.p [] [ H.text "Loading users..." ] ]

            LoadingPosts _ ->
                [ H.p [] [ H.text "Loading posts..." ] ]

            Failed ->
                [ H.p [] [ H.text "Oops, I could not fetch users!" ] ]

            Loaded usersWithPosts ->
                let
                    toLi : UserWithPosts -> Html msg
                    toLi { user, posts } =
                        H.li []
                            [ H.text user.name
                            , H.span [] [ H.text <| " (" ++ (List.length posts |> String.fromInt) ++ " posts)" ]
                            ]
                in
                [ H.h2 [] [ H.text "Got users with posts!" ]
                , H.ul [] <|
                    List.map toLi usersWithPosts
                ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
