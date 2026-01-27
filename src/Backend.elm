module Backend exposing (Model, app, init, update, updateFromFrontendCheckingRights, subscriptions)

import Auth.EmailPasswordAuth as EmailPasswordAuth
import Auth.Flow
import Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Time
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
import Time
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

        GotBitlyResponse connectionIdStr result utmUrl ->
            case result of
                Ok responseBody ->
                    -- Parse the Bitly response to get the short URL
                    case Decode.decodeString bitlyResponseDecoder responseBody of
                        Ok shortUrl ->
                            ( model
                            , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                                (ShortenUrlResult (Ok { utmUrl = utmUrl, shortUrl = shortUrl }))
                            )

                        Err err ->
                            ( model
                            , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                                (ShortenUrlResult (Err ("Failed to parse Bitly response: " ++ Decode.errorToString err)))
                            )

                Err httpErr ->
                    ( model
                    , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                        (ShortenUrlResult (Err (httpErrorToString httpErr)))
                    )

        GotMultiLinkBitlyResponse connectionIdStr email linkId targetClientId destUrl source medium campaign term content tags createdAt result utmUrl ->
            case result of
                Ok responseBody ->
                    case Decode.decodeString bitlyResponseDecoder responseBody of
                        Ok shortUrl ->
                            let
                                newLink : CreatedLink
                                newLink =
                                    { id = linkId
                                    , clientId = targetClientId
                                    , destinationUrl = destUrl
                                    , utmSource = source
                                    , utmMedium = medium
                                    , utmCampaign = campaign
                                    , utmTerm = term
                                    , utmContent = content
                                    , shortUrl = shortUrl
                                    , fullUtmUrl = utmUrl
                                    , tags = tags
                                    , createdAt = createdAt
                                    }

                                -- Update user's links
                                updatedModel =
                                    case Dict.get email model.users of
                                        Just user ->
                                            let
                                                updatedUser =
                                                    { user | links = newLink :: user.links }

                                                updatedUsers =
                                                    Dict.insert email updatedUser model.users
                                            in
                                            { model | users = updatedUsers }

                                        Nothing ->
                                            model
                            in
                            ( updatedModel
                            , Command.batch
                                [ Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                                    (MultiLinkCreationResult [ Ok newLink ])
                                , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                                    (LinksUpdated (case Dict.get email updatedModel.users of
                                        Just user -> user.links
                                        Nothing -> []
                                    ))
                                ]
                            )

                        Err err ->
                            ( model
                            , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                                (MultiLinkCreationResult [ Err ("Failed to parse Bitly response: " ++ Decode.errorToString err) ])
                            )

                Err httpErr ->
                    ( model
                    , Effect.Lamdera.sendToFrontend (Effect.Lamdera.clientIdFromString connectionIdStr)
                        (MultiLinkCreationResult [ Err (httpErrorToString httpErr) ])
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
                        newClient : BitlyClient
                        newClient =
                            { id = user.nextClientId
                            , name = name
                            , apiKey = apiKey
                            , sources = []
                            , tags = []
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

        -- Client Configuration - Sources
        AddSourceToClientBackend targetClientId sourceName ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        newSource =
                            { name = sourceName, isDefault = False }

                        updateClient : BitlyClient -> BitlyClient
                        updateClient c =
                            if c.id == targetClientId then
                                { c | sources = c.sources ++ [ newSource ] }

                            else
                                c

                        updatedClients =
                            List.map updateClient user.clients

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

        RemoveSourceFromClientBackend targetClientId sourceName ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        updateClient : BitlyClient -> BitlyClient
                        updateClient c =
                            if c.id == targetClientId then
                                { c | sources = List.filter (\s -> s.name /= sourceName) c.sources }

                            else
                                c

                        updatedClients =
                            List.map updateClient user.clients

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

        ToggleSourceDefaultBackend targetClientId sourceName ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        toggleDefault : UtmSource -> UtmSource
                        toggleDefault s =
                            if s.name == sourceName then
                                { s | isDefault = not s.isDefault }

                            else
                                s

                        updateClient : BitlyClient -> BitlyClient
                        updateClient c =
                            if c.id == targetClientId then
                                { c | sources = List.map toggleDefault c.sources }

                            else
                                c

                        updatedClients =
                            List.map updateClient user.clients

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

        -- Client Configuration - Tags
        AddTagToClientBackend targetClientId tag ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        updateClient : BitlyClient -> BitlyClient
                        updateClient c =
                            if c.id == targetClientId then
                                { c | tags = c.tags ++ [ tag ] }

                            else
                                c

                        updatedClients =
                            List.map updateClient user.clients

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

        RemoveTagFromClientBackend targetClientId tag ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    let
                        updateClient : BitlyClient -> BitlyClient
                        updateClient c =
                            if c.id == targetClientId then
                                { c | tags = List.filter (\t -> t /= tag) c.tags }

                            else
                                c

                        updatedClients =
                            List.map updateClient user.clients

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

        -- Multi-source link creation
        CreateLinksToBackend targetClientId destUrl utmParams selectedSources selectedTags ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    case List.filter (\c -> c.id == targetClientId) user.clients |> List.head of
                        Just client ->
                            -- Create one link per selected source
                            let
                                sourcesToCreate =
                                    if List.isEmpty selectedSources then
                                        -- Fallback to a single link with the utmParams source
                                        [ utmParams.source ]

                                    else
                                        selectedSources

                                -- For each source, we need to make a Bitly API call
                                -- We'll use the multi-link backend message for handling responses
                                createLinkForSource : Int -> String -> ( Int, Command BackendOnly ToFrontend BackendMsg )
                                createLinkForSource linkId source =
                                    let
                                        fullUtmParams =
                                            { utmParams | source = source }

                                        utmUrl =
                                            buildUtmUrl destUrl fullUtmParams

                                        cmd =
                                            shortenWithBitlyMulti
                                                connectionId
                                                user.email
                                                linkId
                                                targetClientId
                                                destUrl
                                                source
                                                utmParams.medium
                                                utmParams.campaign
                                                utmParams.term
                                                utmParams.content
                                                selectedTags
                                                client.apiKey
                                                utmUrl
                                    in
                                    ( linkId + 1, cmd )

                                ( finalLinkId, commands ) =
                                    List.foldl
                                        (\source ( currentId, cmds ) ->
                                            let
                                                ( nextId, cmd ) =
                                                    createLinkForSource currentId source
                                            in
                                            ( nextId, cmds ++ [ cmd ] )
                                        )
                                        ( user.nextLinkId, [] )
                                        sourcesToCreate

                                -- Update user's nextLinkId
                                updatedUser =
                                    { user | nextLinkId = finalLinkId }

                                updatedUsers =
                                    Dict.insert user.email updatedUser model.users
                            in
                            ( { model | users = updatedUsers }
                            , Command.batch commands
                            )

                        Nothing ->
                            ( model
                            , Effect.Lamdera.sendToFrontend clientId (ShortenUrlResult (Err "Client not found"))
                            )

                Nothing ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (ShortenUrlResult (Err "Not logged in"))
                    )

        GetLinksForClientBackend _ ->
            case getUserFromCookie browserCookie model of
                Just user ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (LinksUpdated user.links)
                    )

                Nothing ->
                    ( model, Command.none )


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
    , links = user.links
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
                , resolver = Http.stringResolver responseStringToResultWithBody
                , timeout = Just 30000
                }
    in
    Task.attempt (\result -> GotBitlyResponse clientIdStr result longUrl) task
        |> Command.fromCmd "shortenWithBitly"


shortenWithBitlyMulti : ConnectionId -> Email -> LinkId -> BitlyClientId -> String -> String -> String -> String -> String -> String -> List String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
shortenWithBitlyMulti connectionIdStr email linkId targetClientId destUrl source medium campaign term content tags apiKey longUrl =
    let
        body =
            Encode.object
                [ ( "long_url", Encode.string longUrl )
                ]

        httpTask =
            Http.task
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ apiKey)
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = "https://api-ssl.bitly.com/v4/shorten"
                , body = Http.jsonBody body
                , resolver = Http.stringResolver responseStringToResultWithBody
                , timeout = Just 30000
                }

        -- Chain HTTP request with getting current time
        combinedTask =
            httpTask
                |> Task.andThen
                    (\httpResult ->
                        Time.now
                            |> Task.map (\now -> ( now, httpResult ))
                    )
    in
    combinedTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok ( now, httpResult ) ->
                        GotMultiLinkBitlyResponse connectionIdStr email linkId targetClientId destUrl source medium campaign term content tags now (Ok httpResult) longUrl

                    Err httpErr ->
                        GotMultiLinkBitlyResponse connectionIdStr email linkId targetClientId destUrl source medium campaign term content tags (Time.millisToPosix 0) (Err httpErr) longUrl
            )
        |> Command.fromCmd "shortenWithBitlyMulti"


bitlyResponseDecoder : Decode.Decoder String
bitlyResponseDecoder =
    Decode.field "link" Decode.string
