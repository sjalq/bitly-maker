module Backend exposing (Model, app, init, update, updateFromFrontendCheckingRights, subscriptions)

import Auth.EmailPasswordAuth as EmailPasswordAuth
import Auth.Flow
import Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Env
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import Logger
import Rights.Auth0 exposing (backendConfig)
import Rights.Permissions exposing (sessionCanPerformAction)
import Rights.Role exposing (roleToString)
import Rights.User exposing (createUser, getUserRole, insertUser, isSysAdmin)
import Supplemental exposing (..)
import Task
import TestData
import Types exposing (..)
import Url


type alias Model =
    BackendModel


app =
    Effect.Lamdera.backend Lamdera.broadcast Lamdera.sendToFrontend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontendCheckingRights
        , subscriptions = subscriptions
        }


subscriptions : Model -> Subscription BackendOnly BackendMsg
subscriptions _ =
    Subscription.none


init : ( Model, Command BackendOnly ToFrontend BackendMsg )
init =
    let
        logSize =
            Env.logSize |> String.toInt |> Maybe.withDefault 2000

        initialModel =
            { logState = Logger.init logSize
            , pendingAuths = Dict.empty
            , sessions = Dict.empty
            , users = Dict.empty
            , emailPasswordCredentials = Dict.empty
            , pollingJobs = Dict.empty
            }

        -- Initialize with test data for development
        modelWithTestData =
            TestData.initializeTestData initialModel
    in
    ( modelWithTestData, Command.none )


update : BackendMsg -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )

        GotLogTime loggerMsg ->
            ( { model | logState = Logger.handleMsg loggerMsg model.logState }
            , Command.none
            )

        GotRemoteModel result ->
            case result of
                Ok model_ ->
                    Logger.logInfo "GotRemoteModel Ok" GotLogTime ( model_, Cmd.none )
                        |> wrapLogCmd

                Err err ->
                    Logger.logError ("GotRemoteModel Err: " ++ httpErrorToString err) GotLogTime ( model, Cmd.none )
                        |> wrapLogCmd

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (backendConfig model) authMsg
                |> Tuple.mapSecond (Command.fromCmd "AuthBackendMsg")

        EmailPasswordAuthResult result ->
            case result of
                EmailPasswordSignupWithHash browserCookie connectionId email password maybeName salt hash ->
                    EmailPasswordAuth.completeSignup browserCookie connectionId email password maybeName salt hash model
                        |> Tuple.mapSecond (Command.fromCmd "EmailPasswordAuth")

        GotBitlyResponse clientId result utmUrl ->
            case result of
                Ok responseBody ->
                    -- Parse the Bitly response to get the short URL
                    case Decode.decodeString bitlyResponseDecoder responseBody of
                        Ok shortUrl ->
                            ( model
                            , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString clientId)
                                (ShortenUrlResult (Ok { utmUrl = utmUrl, shortUrl = shortUrl }))
                            )

                        Err err ->
                            ( model
                            , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString clientId)
                                (ShortenUrlResult (Err ("Failed to parse Bitly response: " ++ Decode.errorToString err)))
                            )

                Err httpErr ->
                    ( model
                    , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString clientId)
                        (ShortenUrlResult (Err (httpErrorToString httpErr)))
                    )


{-| Helper to wrap Logger Cmd results into Command
-}
wrapLogCmd : ( Model, Cmd BackendMsg ) -> ( Model, Command BackendOnly ToFrontend BackendMsg )
wrapLogCmd ( m, cmd ) =
    ( m, Command.fromCmd "logger" cmd )


updateFromFrontend : Effect.Lamdera.SessionId -> Effect.Lamdera.ClientId -> ToBackend -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        -- Convert Effect types to strings for compatibility with existing code
        browserCookie =
            Effect.Lamdera.sessionIdToString sessionId

        connectionId =
            Effect.Lamdera.clientIdToString clientId
    in
    case msg of
        NoOpToBackend ->
            ( model, Command.none )

        Admin_FetchLogs searchQuery ->
            let
                allLogs =
                    Logger.toList model.logState

                filteredLogs =
                    if String.isEmpty searchQuery then
                        allLogs

                    else
                        allLogs
                            |> List.filter
                                (\logEntry ->
                                    String.contains (String.toLower searchQuery)
                                        (String.toLower logEntry.message)
                                )
            in
            ( model, Effect.Lamdera.sendToFrontend clientId (Admin_Logs_ToFrontend filteredLogs) )

        Admin_ClearLogs ->
            let
                logSize =
                    Env.logSize |> String.toInt |> Maybe.withDefault 2000

                newModel =
                    { model | logState = Logger.init logSize }
            in
            ( newModel, Effect.Lamdera.sendToFrontend clientId (Admin_Logs_ToFrontend []) )

        Admin_FetchRemoteModel _ ->
            -- Remote model fetching removed (was RPC-based)
            ( model, Command.none )

        AuthToBackend authToBackend ->
            Auth.Flow.updateFromFrontend (backendConfig model) connectionId browserCookie authToBackend model
                |> Tuple.mapSecond (Command.fromCmd "AuthToBackend")

        EmailPasswordAuthToBackend authMsg ->
            handleEmailPasswordAuth browserCookie connectionId authMsg model
                |> Tuple.mapSecond (Command.fromCmd "EmailPasswordAuth")

        GetUserToBackend ->
            case Dict.get browserCookie model.sessions of
                Just userInfo ->
                    case getUserFromCookie browserCookie model of
                        Just user ->
                            ( model, Command.batch [ Effect.Lamdera.sendToFrontend clientId <| UserInfoMsg <| Just userInfo, Effect.Lamdera.sendToFrontend clientId <| UserDataToFrontend <| userToFrontend user ] )

                        Nothing ->
                            let
                                initialPreferences =
                                    { darkMode = True }

                                -- Default new users to dark mode
                                user =
                                    createUser userInfo initialPreferences

                                newModel =
                                    insertUser userInfo.email user model
                            in
                            ( newModel, Command.batch [ Effect.Lamdera.sendToFrontend clientId <| UserInfoMsg <| Just userInfo, Effect.Lamdera.sendToFrontend clientId <| UserDataToFrontend <| userToFrontend user ] )

                Nothing ->
                    ( model, Effect.Lamdera.sendToFrontend clientId <| UserInfoMsg Nothing )

        LoggedOut ->
            ( { model | sessions = Dict.remove browserCookie model.sessions }, Command.none )

        SetDarkModePreference preference ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        -- Explicitly alias the nested record
                        currentPreferences =
                            user.preferences

                        updatedUserPreferences : Preferences
                        updatedUserPreferences =
                            { currentPreferences | darkMode = preference }

                        -- Update the alias
                        updatedUser : User
                        updatedUser =
                            { user | preferences = updatedUserPreferences }

                        updatedUsers =
                            Dict.insert user.email updatedUser model.users
                    in
                    ( { model | users = updatedUsers }, Command.none )

                Nothing ->
                    Logger.logWarn "User or session not found for SetDarkModePreference" GotLogTime ( model, Cmd.none )
                        |> wrapLogCmd

        A message ->
            -- Echo websocket message back to frontend
            ( model, Effect.Lamdera.sendToFrontend clientId (A0 ("Echo: " ++ message)) )

        AddClientToBackend name apiKey ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        newClient =
                            { id = user.nextClientId
                            , name = name
                            , apiKey = apiKey
                            }

                        updatedUser =
                            { user
                                | clients = user.clients ++ [ newClient ]
                                , nextClientId = user.nextClientId + 1
                            }

                        updatedUsers =
                            Dict.insert user.email updatedUser model.users
                    in
                    ( { model | users = updatedUsers }
                    , Effect.Lamdera.sendToFrontend clientId (ClientsUpdated updatedUser.clients)
                    )

                Nothing ->
                    ( model, Command.none )

        DeleteClientToBackend clientIdToDelete ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        updatedClients =
                            List.filter (\c -> c.id /= clientIdToDelete) user.clients

                        updatedUser =
                            { user | clients = updatedClients }

                        updatedUsers =
                            Dict.insert user.email updatedUser model.users
                    in
                    ( { model | users = updatedUsers }
                    , Effect.Lamdera.sendToFrontend clientId (ClientsUpdated updatedClients)
                    )

                Nothing ->
                    ( model, Command.none )

        ShortenUrlToBackend selectedClientId url utmParams ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    case List.filter (\c -> c.id == selectedClientId) user.clients |> List.head of
                        Just client ->
                            let
                                utmUrl =
                                    buildUtmUrl url utmParams

                                cmd =
                                    shortenWithBitly connectionId client.apiKey utmUrl
                            in
                            ( model, cmd )

                        Nothing ->
                            ( model
                            , Effect.Lamdera.sendToFrontend clientId (ShortenUrlResult (Err "Client not found"))
                            )

                Nothing ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (ShortenUrlResult (Err "Not logged in"))
                    )


updateFromFrontendCheckingRights : Effect.Lamdera.SessionId -> Effect.Lamdera.ClientId -> ToBackend -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendCheckingRights sessionId clientId msg model =
    let
        browserCookie =
            Effect.Lamdera.sessionIdToString sessionId

        isLoggedIn =
            Dict.member browserCookie model.sessions
    in
    if
        case msg of
            NoOpToBackend ->
                True

            LoggedOut ->
                True

            AuthToBackend _ ->
                True

            EmailPasswordAuthToBackend _ ->
                True

            GetUserToBackend ->
                True

            SetDarkModePreference _ ->
                -- Allow everyone to set their own preference
                True

            AddClientToBackend _ _ ->
                -- Only logged in users can add clients
                isLoggedIn

            DeleteClientToBackend _ ->
                -- Only logged in users can delete clients
                isLoggedIn

            ShortenUrlToBackend _ _ _ ->
                -- Only logged in users can shorten URLs
                isLoggedIn

            _ ->
                sessionCanPerformAction model browserCookie msg
    then
        updateFromFrontend sessionId clientId msg model

    else
        ( model, Effect.Lamdera.sendToFrontend clientId (PermissionDenied msg) )


getUserFromCookie : BrowserCookie -> Model -> Maybe User
getUserFromCookie browserCookie model =
    Dict.get browserCookie model.sessions
        |> Maybe.andThen (\userInfo -> Dict.get userInfo.email model.users)


userToFrontend : User -> UserFrontend
userToFrontend user =
    { email = user.email
    , isSysAdmin = isSysAdmin user
    , role = getUserRole user |> roleToString
    , preferences = user.preferences
    , clients = user.clients
    }


handleEmailPasswordAuth : BrowserCookie -> ConnectionId -> EmailPasswordAuthToBackend -> Model -> ( Model, Cmd BackendMsg )
handleEmailPasswordAuth browserCookie connectionId authMsg model =
    case authMsg of
        EmailPasswordLoginToBackend email password ->
            EmailPasswordAuth.handleLogin browserCookie connectionId email password model

        EmailPasswordSignupToBackend email password maybeName ->
            EmailPasswordAuth.handleSignup browserCookie connectionId email password maybeName model



-- UTM URL Building


buildUtmUrl : String -> UtmParams -> String
buildUtmUrl baseUrl params =
    let
        -- Parse the base URL to handle existing query params
        utmQueryParams =
            [ ( "utm_source", params.source )
            , ( "utm_medium", params.medium )
            , ( "utm_campaign", params.campaign )
            , ( "utm_term", params.term )
            , ( "utm_content", params.content )
            ]
                |> List.filter (\( _, v ) -> not (String.isEmpty (String.trim v)))
                |> List.map (\( k, v ) -> Url.percentEncode k ++ "=" ++ Url.percentEncode v)
                |> String.join "&"

        separator =
            if String.contains "?" baseUrl then
                "&"

            else
                "?"
    in
    if String.isEmpty utmQueryParams then
        baseUrl

    else
        baseUrl ++ separator ++ utmQueryParams



-- Bitly API Integration


shortenWithBitly : ConnectionId -> String -> String -> Command BackendOnly ToFrontend BackendMsg
shortenWithBitly clientIdStr apiKey longUrl =
    let
        body =
            Encode.object
                [ ( "long_url", Encode.string longUrl )
                ]

        task =
            Http.task
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ apiKey)
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = "https://api-ssl.bitly.com/v4/shorten"
                , body = Http.jsonBody body
                , resolver = Http.stringResolver responseStringToResult
                , timeout = Just 30000
                }
    in
    Task.attempt (\result -> GotBitlyResponse clientIdStr result longUrl) task
        |> Command.fromCmd "shortenWithBitly"


bitlyResponseDecoder : Decode.Decoder String
bitlyResponseDecoder =
    Decode.field "link" Decode.string
