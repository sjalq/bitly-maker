module Types exposing (AdminLogsUrlParams, AdminPageModel, AdminRoute(..), BackendModel, BackendMsg(..), BitlyClient, BitlyClientId, BrowserCookie, ConnectionId, CreatedLink, DashboardColumn(..), Email, EmailPasswordAuthMsg(..), EmailPasswordAuthResult(..), EmailPasswordAuthToBackend(..), EmailPasswordCredentials, EmailPasswordFormModel, EmailPasswordFormMsg(..), FrontendModel, FrontendMsg(..), LinkId, LoginState(..), PollData, PollingStatus(..), PollingToken, Preferences, Role(..), Route(..), ShortenResult, SortDirection(..), ToBackend(..), ToFrontend(..), UtmParams, UtmSource, User, UserFrontend)

import Auth.Common
import Browser exposing (UrlRequest)
import Dict exposing (Dict)
import Effect.Browser.Navigation
import Http
import Lamdera
import Logger
import Set exposing (Set)
import Time
import Url exposing (Url)



{- Represents a currently connection to a Lamdera client -}


type alias ConnectionId =
    Lamdera.ClientId



{- Represents the browser cookie Lamdera uses to identify a browser -}


type alias BrowserCookie =
    Lamdera.SessionId


type Route
    = Default
    | Dashboard
    | Admin AdminRoute
    | NotFound


type AdminRoute
    = AdminDefault
    | AdminLogs AdminLogsUrlParams
    | AdminFetchModel



-- | AdminFusion


type alias AdminLogsUrlParams =
    { page : Int
    , pageSize : Int
    , search : String
    }


type alias AdminPageModel =
    { logs : List Logger.LogEntry
    , isAuthenticated : Bool
    , remoteUrl : String
    }


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , currentRoute : Route
    , adminPage : AdminPageModel
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    , login : LoginState
    , currentUser : Maybe UserFrontend
    , pendingAuth : Bool
    , preferences : Preferences
    , emailPasswordForm : EmailPasswordFormModel
    , profileDropdownOpen : Bool
    , loginModalOpen : Bool

    -- UTM Builder state
    , clients : List BitlyClient
    , selectedClientId : Maybe BitlyClientId
    , destinationUrl : String
    , utmSource : String
    , utmMedium : String
    , utmCampaign : String
    , utmTerm : String
    , utmContent : String
    , shortenResults : List ShortenResult
    , isShortening : Bool

    -- Client management
    , newClientName : String
    , newClientApiKey : String
    , clientFormError : Maybe String
    , showClientManager : Bool

    -- Client configuration (sources/tags management)
    , newSourceInput : String
    , newTagInput : String
    , editingClientId : Maybe BitlyClientId

    -- Link creation with sources
    , selectedSources : Set String -- Which sources are checked
    , selectedTags : Set String -- Which tags are selected

    -- Dashboard state
    , linksForDashboard : List CreatedLink
    , dashboardSortColumn : DashboardColumn
    , dashboardSortDirection : SortDirection
    , dashboardFilters : Dict String String -- column name -> filter value
    }


type alias EmailPasswordFormModel =
    { email : String
    , password : String
    , confirmPassword : String
    , name : String
    , isSignupMode : Bool
    , error : Maybe String
    }


type alias BackendModel =
    { logState : Logger.LogState
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , sessions : Dict Lamdera.SessionId Auth.Common.UserInfo
    , users : Dict Email User
    , emailPasswordCredentials : Dict Email EmailPasswordCredentials
    , pollingJobs : Dict PollingToken (PollingStatus PollData)
    }


type alias EmailPasswordCredentials =
    { email : String
    , passwordHash : String
    , passwordSalt : String
    , createdAt : Int
    }


type EmailPasswordAuthMsg
    = EmailPasswordFormMsg EmailPasswordFormMsg
    | EmailPasswordLoginRequested String String
    | EmailPasswordSignupRequested String String (Maybe String)


type EmailPasswordFormMsg
    = EmailPasswordFormEmailChanged String
    | EmailPasswordFormPasswordChanged String
    | EmailPasswordFormConfirmPasswordChanged String
    | EmailPasswordFormNameChanged String
    | EmailPasswordFormToggleMode
    | EmailPasswordFormSubmit


type EmailPasswordAuthToBackend
    = EmailPasswordLoginToBackend String String
    | EmailPasswordSignupToBackend String String (Maybe String)


type EmailPasswordAuthResult
    = EmailPasswordSignupWithHash BrowserCookie ConnectionId String String (Maybe String) String String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UrlRequested UrlRequest
    | NoOpFrontendMsg
    | DirectToBackend ToBackend
      --- Admin
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
      -- UTM Builder
    | SelectClient (Maybe BitlyClientId)
    | DestinationUrlChanged String
    | UtmSourceChanged String
    | UtmMediumChanged String
    | UtmCampaignChanged String
    | UtmTermChanged String
    | UtmContentChanged String
    | CreateShortLink
    | ClearForm
      -- Client Management
    | NewClientNameChanged String
    | NewClientApiKeyChanged String
    | AddClient
    | DeleteClient BitlyClientId
    | ToggleClientManager
      -- Client Configuration (sources/tags)
    | NewSourceInputChanged String
    | AddSourceToClient BitlyClientId
    | RemoveSourceFromClient BitlyClientId String
    | ToggleSourceDefault BitlyClientId String -- Toggle isDefault flag on a source
    | NewTagInputChanged String
    | AddTagToClient BitlyClientId
    | RemoveTagFromClient BitlyClientId String
    | ToggleSourceSelection String -- For link creation checkboxes
    | ToggleTagSelection String
    | SetEditingClient (Maybe BitlyClientId)
      -- Dashboard
    | SetDashboardSort DashboardColumn
    | SetDashboardFilter String String
    | ClearDashboardFilters



--- Fusion
-- | Admin_FusionPatch Fusion.Patch.Patch
-- | Admin_FusionQuery Fusion.Query


type ToBackend
    = A String -- WebSocket message from JS (guaranteed tag 0)
    | Admin_ClearLogs
    | Admin_FetchLogs String -- Search query parameter
    | Admin_FetchRemoteModel String
    | AuthToBackend Auth.Common.ToBackend
    | EmailPasswordAuthToBackend EmailPasswordAuthToBackend
    | GetUserToBackend
    | LoggedOut
    | NoOpToBackend
    | SetDarkModePreference Bool
      -- Client Management
    | AddClientToBackend String String -- name, apiKey
    | DeleteClientToBackend BitlyClientId
      -- URL Shortening
    | ShortenUrlToBackend BitlyClientId String UtmParams -- clientId, url, utm params
      -- Client Configuration (sources/tags)
    | AddSourceToClientBackend BitlyClientId String
    | RemoveSourceFromClientBackend BitlyClientId String
    | ToggleSourceDefaultBackend BitlyClientId String
    | AddTagToClientBackend BitlyClientId String
    | RemoveTagFromClientBackend BitlyClientId String
      -- Multi-source link creation
    | CreateLinksToBackend BitlyClientId String UtmParams (List String) (List String) -- clientId, destUrl, utmParams (medium/campaign/term/content), selectedSources, selectedTags
    | GetLinksForClientBackend BitlyClientId



--- Fusion
-- | Fusion_PersistPatch Fusion.Patch.Patch
-- | Fusion_Query Fusion.Query


type BackendMsg
    = NoOpBackendMsg
    | GotLogTime Logger.Msg
    | GotRemoteModel (Result Http.Error BackendModel)
    | AuthBackendMsg Auth.Common.BackendMsg
    | EmailPasswordAuthResult EmailPasswordAuthResult
      -- Bitly API response
    | GotBitlyResponse ConnectionId (Result Http.Error String) String -- clientId, result, utmUrl
      -- Multi-link creation
    | GotMultiLinkBitlyResponse ConnectionId Email LinkId BitlyClientId String String String String String String (List String) Time.Posix (Result Http.Error String) String
      -- connectionId, email, linkId, clientId, destUrl, source, medium, campaign, term, content, tags, createdAt, result, utmUrl


type ToFrontend
    = A0 String -- WebSocket message to JS (guaranteed tag 0, '0' < 'A' so A0 < Admin...)
    | Admin_Logs_ToFrontend (List Logger.LogEntry)
    | AuthSuccess Auth.Common.UserInfo
    | AuthToFrontend Auth.Common.ToFrontend
    | NoOpToFrontend
    | PermissionDenied ToBackend
    | UserDataToFrontend UserFrontend
    | UserInfoMsg (Maybe Auth.Common.UserInfo)
      -- Client Management
    | ClientsUpdated (List BitlyClient)
      -- URL Shortening
    | ShortenUrlResult (Result String ShortenResult)
      -- Links dashboard
    | LinksUpdated (List CreatedLink)
      -- Multi-link creation result
    | MultiLinkCreationResult (List (Result String CreatedLink))



-- | Admin_FusionResponse Fusion.Value


type alias Email =
    String


type alias BitlyClientId =
    Int


type alias UtmSource =
    { name : String
    , isDefault : Bool -- If true, pre-checked when creating links
    }


type alias BitlyClient =
    { id : BitlyClientId
    , name : String
    , apiKey : String
    , sources : List UtmSource -- Sources with default flag
    , tags : List String -- e.g., ["Q1-Campaign", "Product-Launch"]
    }


type alias UtmParams =
    { source : String
    , medium : String
    , campaign : String
    , term : String
    , content : String
    }


type alias ShortenResult =
    { utmUrl : String
    , shortUrl : String
    }


type alias LinkId =
    Int


type alias CreatedLink =
    { id : LinkId -- Auto-increment per user
    , clientId : BitlyClientId -- Which client created this
    , destinationUrl : String -- Original URL
    , utmSource : String -- The source used
    , utmMedium : String
    , utmCampaign : String
    , utmTerm : String
    , utmContent : String
    , shortUrl : String -- Bitly shortened URL
    , fullUtmUrl : String -- Full URL with UTM params
    , tags : List String -- Tags applied to this link
    , createdAt : Time.Posix -- When created
    }


type alias User =
    { email : Email
    , name : Maybe String
    , preferences : Preferences
    , clients : List BitlyClient
    , nextClientId : BitlyClientId
    , links : List CreatedLink -- All created links
    , nextLinkId : LinkId -- Counter for link IDs
    }


type alias UserFrontend =
    { email : Email
    , isSysAdmin : Bool
    , role : String
    , preferences : Preferences
    , clients : List BitlyClient
    , links : List CreatedLink
    }


type LoginState
    = JustArrived
    | NotLogged Bool
    | LoginTokenSent
    | LoggedIn Auth.Common.UserInfo



-- Role types


type Role
    = SysAdmin
    | UserRole
    | Anonymous



-- Polling types


type alias PollingToken =
    String


type PollingStatus a
    = Busy
    | BusyWithTime Int
    | Ready (Result String a)


type alias PollData =
    String



-- Dashboard types


type DashboardColumn
    = ColCreatedAt
    | ColDestinationUrl
    | ColSource
    | ColMedium
    | ColCampaign
    | ColShortUrl
    | ColTags


type SortDirection
    = Ascending
    | Descending



-- USER RELATED TYPES


type alias Preferences =
    { darkMode : Bool
    }
