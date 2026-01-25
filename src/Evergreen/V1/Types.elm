module Evergreen.V1.Types exposing (..)

import Browser
import Dict
import Effect.Browser.Navigation
import Evergreen.V1.Auth.Common
import Evergreen.V1.Logger
import Http
import Lamdera
import Url


type alias AdminLogsUrlParams =
    { page : Int
    , pageSize : Int
    , search : String
    }


type AdminRoute
    = AdminDefault
    | AdminLogs AdminLogsUrlParams
    | AdminFetchModel


type Route
    = Default
    | Admin AdminRoute
    | NotFound


type alias AdminPageModel =
    { logs : List Evergreen.V1.Logger.LogEntry
    , isAuthenticated : Bool
    , remoteUrl : String
    }


type LoginState
    = JustArrived
    | NotLogged Bool
    | LoginTokenSent
    | LoggedIn Evergreen.V1.Auth.Common.UserInfo


type alias Email =
    String


type alias Preferences =
    { darkMode : Bool
    }


type alias BitlyClientId =
    Int


type alias BitlyClient =
    { id : BitlyClientId
    , name : String
    , apiKey : String
    }


type alias UserFrontend =
    { email : Email
    , isSysAdmin : Bool
    , role : String
    , preferences : Preferences
    , clients : List BitlyClient
    }


type alias EmailPasswordFormModel =
    { email : String
    , password : String
    , confirmPassword : String
    , name : String
    , isSignupMode : Bool
    , error : Maybe String
    }


type alias ShortenResult =
    { utmUrl : String
    , shortUrl : String
    }


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , currentRoute : Route
    , adminPage : AdminPageModel
    , authFlow : Evergreen.V1.Auth.Common.Flow
    , authRedirectBaseUrl : Url.Url
    , login : LoginState
    , currentUser : Maybe UserFrontend
    , pendingAuth : Bool
    , preferences : Preferences
    , emailPasswordForm : EmailPasswordFormModel
    , profileDropdownOpen : Bool
    , loginModalOpen : Bool
    , clients : List BitlyClient
    , selectedClientId : Maybe BitlyClientId
    , destinationUrl : String
    , utmSource : String
    , utmMedium : String
    , utmCampaign : String
    , utmTerm : String
    , utmContent : String
    , shortenResult : Maybe ShortenResult
    , isShortening : Bool
    , newClientName : String
    , newClientApiKey : String
    , clientFormError : Maybe String
    , showClientManager : Bool
    }


type alias User =
    { email : Email
    , name : Maybe String
    , preferences : Preferences
    , clients : List BitlyClient
    , nextClientId : BitlyClientId
    }


type alias EmailPasswordCredentials =
    { email : String
    , passwordHash : String
    , passwordSalt : String
    , createdAt : Int
    }


type alias PollingToken =
    String


type alias PollData =
    String


type PollingStatus a
    = Busy
    | BusyWithTime Int
    | Ready (Result String a)


type alias BackendModel =
    { logState : Evergreen.V1.Logger.LogState
    , pendingAuths : Dict.Dict Lamdera.SessionId Evergreen.V1.Auth.Common.PendingAuth
    , sessions : Dict.Dict Lamdera.SessionId Evergreen.V1.Auth.Common.UserInfo
    , users : Dict.Dict Email User
    , emailPasswordCredentials : Dict.Dict Email EmailPasswordCredentials
    , pollingJobs : Dict.Dict PollingToken (PollingStatus PollData)
    }


type EmailPasswordAuthToBackend
    = EmailPasswordLoginToBackend String String
    | EmailPasswordSignupToBackend String String (Maybe String)


type alias UtmParams =
    { source : String
    , medium : String
    , campaign : String
    , term : String
    , content : String
    }


type ToBackend
    = A String
    | Admin_ClearLogs
    | Admin_FetchLogs String
    | Admin_FetchRemoteModel String
    | AuthToBackend Evergreen.V1.Auth.Common.ToBackend
    | EmailPasswordAuthToBackend EmailPasswordAuthToBackend
    | GetUserToBackend
    | LoggedOut
    | NoOpToBackend
    | SetDarkModePreference Bool
    | AddClientToBackend String String
    | DeleteClientToBackend BitlyClientId
    | ShortenUrlToBackend BitlyClientId String UtmParams


type EmailPasswordFormMsg
    = EmailPasswordFormEmailChanged String
    | EmailPasswordFormPasswordChanged String
    | EmailPasswordFormConfirmPasswordChanged String
    | EmailPasswordFormNameChanged String
    | EmailPasswordFormToggleMode
    | EmailPasswordFormSubmit


type EmailPasswordAuthMsg
    = EmailPasswordFormMsg EmailPasswordFormMsg
    | EmailPasswordLoginRequested String String
    | EmailPasswordSignupRequested String String (Maybe String)


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | NoOpFrontendMsg
    | DirectToBackend ToBackend
    | Admin_RemoteUrlChanged String
    | Admin_LogsNavigate AdminLogsUrlParams
    | Auth0SigninRequested
    | EmailPasswordAuthMsg EmailPasswordAuthMsg
    | Logout
    | ToggleDarkMode
    | ToggleProfileDropdown
    | ToggleLoginModal
    | CloseLoginModal
    | EmailPasswordAuthError String
    | CopyToClipboard String
    | ClipboardResult (Result String String)
    | SelectClient (Maybe BitlyClientId)
    | DestinationUrlChanged String
    | UtmSourceChanged String
    | UtmMediumChanged String
    | UtmCampaignChanged String
    | UtmTermChanged String
    | UtmContentChanged String
    | CreateShortLink
    | ClearForm
    | NewClientNameChanged String
    | NewClientApiKeyChanged String
    | AddClient
    | DeleteClient BitlyClientId
    | ToggleClientManager


type alias BrowserCookie =
    Lamdera.SessionId


type alias ConnectionId =
    Lamdera.ClientId


type EmailPasswordAuthResult
    = EmailPasswordSignupWithHash BrowserCookie ConnectionId String String (Maybe String) String String


type BackendMsg
    = NoOpBackendMsg
    | GotLogTime Evergreen.V1.Logger.Msg
    | GotRemoteModel (Result Http.Error BackendModel)
    | AuthBackendMsg Evergreen.V1.Auth.Common.BackendMsg
    | EmailPasswordAuthResult EmailPasswordAuthResult
    | GotBitlyResponse ConnectionId (Result Http.Error String) String


type ToFrontend
    = A0 String
    | Admin_Logs_ToFrontend (List Evergreen.V1.Logger.LogEntry)
    | AuthSuccess Evergreen.V1.Auth.Common.UserInfo
    | AuthToFrontend Evergreen.V1.Auth.Common.ToFrontend
    | NoOpToFrontend
    | PermissionDenied ToBackend
    | UserDataToFrontend UserFrontend
    | UserInfoMsg (Maybe Evergreen.V1.Auth.Common.UserInfo)
    | ClientsUpdated (List BitlyClient)
    | ShortenUrlResult (Result String ShortenResult)
