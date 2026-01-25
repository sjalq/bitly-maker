module Pages.Default exposing (..)

import Components.Button
import Components.Card
import Components.Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as HE
import Theme
import Types exposing (..)


init : FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
init model =
    ( model, Cmd.none )


view : FrontendModel -> Theme.Colors -> Html FrontendMsg
view model colors =
    div [ Attr.style "background-color" colors.primaryBg, Attr.class "min-h-screen" ]
        [ div [ Attr.class "container mx-auto px-4 md:px-6 py-4 md:py-8 max-w-2xl" ]
            [ Components.Header.pageHeader colors "Bitly UTM Maker" (Just "Create shortened links with UTM tracking")
            , case model.login of
                LoggedIn _ ->
                    viewUtmBuilder model colors

                _ ->
                    viewLoginPrompt colors
            ]
        ]


viewLoginPrompt : Theme.Colors -> Html FrontendMsg
viewLoginPrompt colors =
    div [ Attr.class "mt-8" ]
        [ Components.Card.simple colors
            [ p [ Attr.class "text-center", Attr.style "color" colors.primaryText ]
                [ text "Please log in to create shortened links with your Bitly clients." ]
            , div [ Attr.class "mt-4 text-center" ]
                [ Components.Button.primary colors (Just ToggleLoginModal) "Log In" ]
            ]
        ]


viewUtmBuilder : FrontendModel -> Theme.Colors -> Html FrontendMsg
viewUtmBuilder model colors =
    div [ Attr.class "mt-6 space-y-6" ]
        [ -- Main UTM Builder Card
          Components.Card.withTitle colors
            "Create Short Link"
            [ -- Client Selection
              div [ Attr.class "mb-4" ]
                [ label
                    [ Attr.class "block text-sm font-medium mb-2"
                    , Attr.style "color" colors.primaryText
                    ]
                    [ text "Client" ]
                , if List.isEmpty model.clients then
                    div []
                        [ p
                            [ Attr.class "text-sm mb-2"
                            , Attr.style "color" colors.mutedText
                            ]
                            [ text "No clients yet. Add a client below to get started." ]
                        ]

                  else
                    select
                        [ Attr.class "w-full p-3 rounded border"
                        , Attr.style "background-color" colors.inputBg
                        , Attr.style "color" colors.primaryText
                        , Attr.style "border-color" colors.border
                        , HE.onInput (\s -> SelectClient (String.toInt s))
                        ]
                        (option [ Attr.value "" ] [ text "-- Select Client --" ]
                            :: List.map (viewClientOption model.selectedClientId) model.clients
                        )
                ]

            -- Destination URL
            , div [ Attr.class "mb-4" ]
                [ label
                    [ Attr.class "block text-sm font-medium mb-2"
                    , Attr.style "color" colors.primaryText
                    ]
                    [ text "Destination URL" ]
                , input
                    [ Attr.type_ "url"
                    , Attr.class "w-full p-3 rounded border"
                    , Attr.style "background-color" colors.inputBg
                    , Attr.style "color" colors.primaryText
                    , Attr.style "border-color" colors.border
                    , Attr.placeholder "https://example.com/page"
                    , Attr.value model.destinationUrl
                    , HE.onInput DestinationUrlChanged
                    ]
                    []
                ]

            -- UTM Parameters Grid
            , div [ Attr.class "grid grid-cols-1 md:grid-cols-2 gap-4 mb-4" ]
                [ viewUtmInput colors "UTM Source" "e.g., google, newsletter" model.utmSource UtmSourceChanged
                , viewUtmInput colors "UTM Medium" "e.g., cpc, email, social" model.utmMedium UtmMediumChanged
                ]
            , div [ Attr.class "mb-4" ]
                [ viewUtmInput colors "UTM Campaign" "e.g., spring_sale, product_launch" model.utmCampaign UtmCampaignChanged
                ]
            , div [ Attr.class "grid grid-cols-1 md:grid-cols-2 gap-4 mb-6" ]
                [ viewUtmInput colors "UTM Term (optional)" "e.g., running+shoes" model.utmTerm UtmTermChanged
                , viewUtmInput colors "UTM Content (optional)" "e.g., logolink, textlink" model.utmContent UtmContentChanged
                ]

            -- Action Buttons
            , div [ Attr.class "flex gap-3" ]
                [ if model.isShortening then
                    Components.Button.loading colors "Creating..."

                  else
                    Components.Button.primary colors (Just CreateShortLink) "Create Short Link"
                , Components.Button.secondary colors (Just ClearForm) "Clear"
                ]

            -- Result Display
            , case model.shortenResult of
                Just result ->
                    viewResult colors result

                Nothing ->
                    text ""
            ]

        -- Client Manager Section
        , viewClientManager model colors

        -- Help Section
        , viewBitlyHelp colors
        ]


viewUtmInput : Theme.Colors -> String -> String -> String -> (String -> FrontendMsg) -> Html FrontendMsg
viewUtmInput colors labelText placeholder value onInput =
    div []
        [ label
            [ Attr.class "block text-sm font-medium mb-2"
            , Attr.style "color" colors.primaryText
            ]
            [ text labelText ]
        , input
            [ Attr.type_ "text"
            , Attr.class "w-full p-3 rounded border"
            , Attr.style "background-color" colors.inputBg
            , Attr.style "color" colors.primaryText
            , Attr.style "border-color" colors.border
            , Attr.placeholder placeholder
            , Attr.value value
            , HE.onInput onInput
            ]
            []
        ]


viewClientOption : Maybe BitlyClientId -> BitlyClient -> Html FrontendMsg
viewClientOption selectedId client =
    option
        [ Attr.value (String.fromInt client.id)
        , Attr.selected (selectedId == Just client.id)
        ]
        [ text client.name ]


viewResult : Theme.Colors -> ShortenResult -> Html FrontendMsg
viewResult colors result =
    div
        [ Attr.class "mt-6 p-4 rounded"
        , Attr.style "background-color" colors.inputBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ -- Short URL
          div [ Attr.class "mb-4" ]
            [ label
                [ Attr.class "block text-xs font-medium mb-1"
                , Attr.style "color" colors.mutedText
                ]
                [ text "Short URL (click to copy)" ]
            , div
                [ Attr.class "p-3 rounded cursor-pointer font-mono text-lg font-bold"
                , Attr.style "background-color" colors.secondaryBg
                , Attr.style "color" "#4ade80"
                , Attr.style "border" "1px solid transparent"
                , HE.onClick (CopyToClipboard result.shortUrl)
                ]
                [ text result.shortUrl ]
            ]

        -- Full UTM URL
        , div []
            [ label
                [ Attr.class "block text-xs font-medium mb-1"
                , Attr.style "color" colors.mutedText
                ]
                [ text "Full UTM URL (click to copy)" ]
            , div
                [ Attr.class "p-3 rounded cursor-pointer font-mono text-xs break-all"
                , Attr.style "background-color" colors.secondaryBg
                , Attr.style "color" colors.mutedText
                , Attr.style "border" "1px solid transparent"
                , HE.onClick (CopyToClipboard result.utmUrl)
                ]
                [ text result.utmUrl ]
            ]
        ]


viewClientManager : FrontendModel -> Theme.Colors -> Html FrontendMsg
viewClientManager model colors =
    Components.Card.simple colors
        [ -- Header with toggle
          div
            [ Attr.class "flex items-center justify-between cursor-pointer"
            , HE.onClick ToggleClientManager
            ]
            [ h2
                [ Attr.class "text-lg font-semibold"
                , Attr.style "color" colors.primaryText
                ]
                [ text "Manage Clients" ]
            , span
                [ Attr.style "color" colors.mutedText ]
                [ text
                    (if model.showClientManager then
                        "▼"

                     else
                        "▶"
                    )
                ]
            ]

        -- Collapsible content
        , if model.showClientManager then
            div [ Attr.class "mt-4" ]
                [ -- Add client form
                  div [ Attr.class "flex flex-col md:flex-row gap-3 mb-4" ]
                    [ div [ Attr.class "flex-1" ]
                        [ input
                            [ Attr.type_ "text"
                            , Attr.class "w-full p-3 rounded border"
                            , Attr.style "background-color" colors.inputBg
                            , Attr.style "color" colors.primaryText
                            , Attr.style "border-color" colors.border
                            , Attr.placeholder "Client name"
                            , Attr.value model.newClientName
                            , HE.onInput NewClientNameChanged
                            ]
                            []
                        ]
                    , div [ Attr.class "flex-1" ]
                        [ input
                            [ Attr.type_ "text"
                            , Attr.class "w-full p-3 rounded border"
                            , Attr.style "background-color" colors.inputBg
                            , Attr.style "color" colors.primaryText
                            , Attr.style "border-color" colors.border
                            , Attr.placeholder "Bitly API token"
                            , Attr.value model.newClientApiKey
                            , HE.onInput NewClientApiKeyChanged
                            ]
                            []
                        ]
                    , Components.Button.secondary colors (Just AddClient) "Add"
                    ]

                -- Error message
                , case model.clientFormError of
                    Just err ->
                        div
                            [ Attr.class "mb-4 p-3 rounded"
                            , Attr.style "background-color" "rgba(239, 68, 68, 0.1)"
                            , Attr.style "color" "#ef4444"
                            ]
                            [ text err ]

                    Nothing ->
                        text ""

                -- Client list
                , if List.isEmpty model.clients then
                    p
                        [ Attr.class "text-center py-4"
                        , Attr.style "color" colors.mutedText
                        ]
                        [ text "No clients yet. Add one above." ]

                  else
                    div [ Attr.class "space-y-2" ]
                        (List.map (viewClientItem colors) model.clients)
                ]

          else
            text ""
        ]


viewClientItem : Theme.Colors -> BitlyClient -> Html FrontendMsg
viewClientItem colors client =
    div
        [ Attr.class "flex items-center justify-between p-3 rounded"
        , Attr.style "background-color" colors.inputBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ span
            [ Attr.class "font-medium"
            , Attr.style "color" colors.primaryText
            ]
            [ text client.name ]
        , Components.Button.view
            { variant = Components.Button.Danger
            , size = Components.Button.Small
            , disabled = False
            , onClick = Just (DeleteClient client.id)
            , colors = colors
            }
            "Delete"
        ]


viewBitlyHelp : Theme.Colors -> Html FrontendMsg
viewBitlyHelp colors =
    Components.Card.simple colors
        [ h2
            [ Attr.class "text-lg font-semibold mb-3"
            , Attr.style "color" colors.primaryText
            ]
            [ text "How to Get a Bitly API Token" ]
        , ol
            [ Attr.class "list-decimal list-inside space-y-2 text-sm"
            , Attr.style "color" colors.mutedText
            ]
            [ li []
                [ text "Go to "
                , a
                    [ Attr.href "https://app.bitly.com/"
                    , Attr.target "_blank"
                    , Attr.rel "noopener noreferrer"
                    , Attr.style "color" "#3b82f6"
                    , Attr.style "text-decoration" "underline"
                    ]
                    [ text "app.bitly.com" ]
                , text " and sign in (or create an account)"
                ]
            , li []
                [ text "Click your profile icon in the top-right corner" ]
            , li []
                [ text "Select "
                , strong [] [ text "Settings" ]
                ]
            , li []
                [ text "In the left sidebar, click "
                , strong [] [ text "Developer settings" ]
                , text " (or go directly to "
                , a
                    [ Attr.href "https://app.bitly.com/settings/api/"
                    , Attr.target "_blank"
                    , Attr.rel "noopener noreferrer"
                    , Attr.style "color" "#3b82f6"
                    , Attr.style "text-decoration" "underline"
                    ]
                    [ text "bitly.com/settings/api" ]
                , text ")"
                ]
            , li []
                [ text "Under "
                , strong [] [ text "Access tokens" ]
                , text ", enter your Bitly password and click "
                , strong [] [ text "Generate token" ]
                , br [] []
                , span [ Attr.class "text-xs", Attr.style "color" colors.mutedText ]
                    [ text "(If you signed up with Google/social login and don't have a password, go to "
                    , strong [] [ text "Account settings" ]
                    , text " → "
                    , strong [] [ text "Security" ]
                    , text " to set one first)"
                    ]
                ]
            , li []
                [ text "Copy the generated token and paste it above when adding a client" ]
            ]
        , p
            [ Attr.class "mt-4 text-xs"
            , Attr.style "color" colors.mutedText
            ]
            [ text "Note: Keep your API token secure. Each client/project should ideally have its own Bitly account and token." ]
        ]
