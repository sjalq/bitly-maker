module Helpers.TestModels exposing
    ( anonymousUser
    , defaultPreferences
    , emptyBackendModel
    , regularUser
    , sysAdminUser
    )

import Auth.Common
import Dict
import Logger
import Types exposing (BackendModel, Preferences, User)



-- USER BUILDERS


anonymousUser : User
anonymousUser =
    { email = "anon@example.com"
    , name = Nothing
    , preferences = defaultPreferences
    , clients = []
    , nextClientId = 1
    }


regularUser : User
regularUser =
    { email = "user@example.com"
    , name = Just "Regular User"
    , preferences = defaultPreferences
    , clients = []
    , nextClientId = 1
    }


sysAdminUser : User
sysAdminUser =
    { email = "sys@admin.com"
    , name = Just "System Administrator"
    , preferences = defaultPreferences
    , clients = []
    , nextClientId = 1
    }


defaultPreferences : Preferences
defaultPreferences =
    { darkMode = True }



-- BACKEND MODEL BUILDERS


emptyBackendModel : BackendModel
emptyBackendModel =
    { logState = Logger.init 100
    , pendingAuths = Dict.empty
    , sessions = Dict.empty
    , users = Dict.empty
    , emailPasswordCredentials = Dict.empty
    , pollingJobs = Dict.empty
    }
