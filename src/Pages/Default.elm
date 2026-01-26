module Pages.Default exposing (..)

import Components.Button
import Components.Card
import Components.Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as HE
import Set exposing (Set)
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
    let
        selectedClient =
            model.selectedClientId
                |> Maybe.andThen
                    (\clientId ->
                        List.filter (\c -> c.id == clientId) model.clients
                            |> List.head
                    )
    in
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

            -- Sources selection (from client)
            , viewSourcesSection model colors selectedClient

            -- UTM Parameters Grid (without Source)
            , div [ Attr.class "mb-4" ]
                [ viewUtmInput colors "UTM Medium" "e.g., cpc, email, social" model.utmMedium UtmMediumChanged
                ]
            , div [ Attr.class "mb-4" ]
                [ viewUtmInput colors "UTM Campaign" "e.g., spring_sale, product_launch" model.utmCampaign UtmCampaignChanged
                ]
            , div [ Attr.class "grid grid-cols-1 md:grid-cols-2 gap-4 mb-4" ]
                [ viewUtmInput colors "UTM Term (optional)" "e.g., running+shoes" model.utmTerm UtmTermChanged
                , viewUtmInput colors "UTM Content (optional)" "e.g., logolink, textlink" model.utmContent UtmContentChanged
                ]

            -- Tags selection (from client)
            , viewTagsSection model colors selectedClient

            -- Action Buttons
            , div [ Attr.class "flex gap-3" ]
                [ if model.isShortening then
                    Components.Button.loading colors "Creating..."

                  else
                    viewCreateButton model colors selectedClient
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


viewSourcesSection : FrontendModel -> Theme.Colors -> Maybe BitlyClient -> Html FrontendMsg
viewSourcesSection model colors maybeClient =
    div [ Attr.class "mb-4" ]
        [ label
            [ Attr.class "block text-sm font-medium mb-2"
            , Attr.style "color" colors.primaryText
            ]
            [ text "UTM Sources" ]
        , case maybeClient of
            Just client ->
                if List.isEmpty client.sources then
                    div []
                        [ p
                            [ Attr.class "text-sm mb-2"
                            , Attr.style "color" colors.mutedText
                            ]
                            [ text "No sources configured. Add sources in the client manager below, or enter a source manually:" ]
                        , input
                            [ Attr.type_ "text"
                            , Attr.class "w-full p-3 rounded border"
                            , Attr.style "background-color" colors.inputBg
                            , Attr.style "color" colors.primaryText
                            , Attr.style "border-color" colors.border
                            , Attr.placeholder "e.g., google, newsletter"
                            , Attr.value model.utmSource
                            , HE.onInput UtmSourceChanged
                            ]
                            []
                        ]

                else
                    div []
                        [ p
                            [ Attr.class "text-xs mb-2"
                            , Attr.style "color" colors.mutedText
                            ]
                            [ text "Select sources to create links for (one link per source):" ]
                        , div [ Attr.class "flex flex-wrap gap-2" ]
                            (List.map (viewSourceCheckbox model.selectedSources colors) client.sources)
                        ]

            Nothing ->
                p
                    [ Attr.class "text-sm"
                    , Attr.style "color" colors.mutedText
                    ]
                    [ text "Select a client to see available sources" ]
        ]


viewSourceCheckbox : Set String -> Theme.Colors -> UtmSource -> Html FrontendMsg
viewSourceCheckbox selectedSources colors source =
    let
        isSelected =
            Set.member source.name selectedSources
    in
    label
        [ Attr.class "flex items-center gap-2 px-3 py-2 rounded cursor-pointer"
        , Attr.style "background-color"
            (if isSelected then
                colors.buttonBg

             else
                colors.inputBg
            )
        , Attr.style "color"
            (if isSelected then
                colors.buttonText

             else
                colors.primaryText
            )
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ input
            [ Attr.type_ "checkbox"
            , Attr.checked isSelected
            , HE.onClick (ToggleSourceSelection source.name)
            , Attr.class "sr-only"
            ]
            []
        , span [ Attr.class "text-sm" ] [ text source.name ]
        , if source.isDefault then
            span
                [ Attr.class "text-xs px-1 rounded"
                , Attr.style "background-color" colors.secondaryBg
                , Attr.style "color" colors.mutedText
                ]
                [ text "default" ]

          else
            text ""
        ]


viewTagsSection : FrontendModel -> Theme.Colors -> Maybe BitlyClient -> Html FrontendMsg
viewTagsSection model colors maybeClient =
    div [ Attr.class "mb-6" ]
        [ label
            [ Attr.class "block text-sm font-medium mb-2"
            , Attr.style "color" colors.primaryText
            ]
            [ text "Tags (optional)" ]
        , case maybeClient of
            Just client ->
                if List.isEmpty client.tags then
                    p
                        [ Attr.class "text-sm"
                        , Attr.style "color" colors.mutedText
                        ]
                        [ text "No tags configured for this client. Add tags in the client manager below." ]

                else
                    div [ Attr.class "flex flex-wrap gap-2" ]
                        (List.map (viewTagCheckbox model.selectedTags colors) client.tags)

            Nothing ->
                p
                    [ Attr.class "text-sm"
                    , Attr.style "color" colors.mutedText
                    ]
                    [ text "Select a client to see available tags" ]
        ]


viewTagCheckbox : Set String -> Theme.Colors -> String -> Html FrontendMsg
viewTagCheckbox selectedTags colors tag =
    let
        isSelected =
            Set.member tag selectedTags
    in
    label
        [ Attr.class "flex items-center gap-2 px-3 py-2 rounded cursor-pointer"
        , Attr.style "background-color"
            (if isSelected then
                colors.buttonBg

             else
                colors.inputBg
            )
        , Attr.style "color"
            (if isSelected then
                colors.buttonText

             else
                colors.primaryText
            )
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ input
            [ Attr.type_ "checkbox"
            , Attr.checked isSelected
            , HE.onClick (ToggleTagSelection tag)
            , Attr.class "sr-only"
            ]
            []
        , span [ Attr.class "text-sm" ] [ text tag ]
        ]


viewCreateButton : FrontendModel -> Theme.Colors -> Maybe BitlyClient -> Html FrontendMsg
viewCreateButton model colors maybeClient =
    let
        selectedSourceCount =
            Set.size model.selectedSources

        -- Determine which sources to use
        sourcesToUse =
            case maybeClient of
                Just client ->
                    if List.isEmpty client.sources then
                        -- No sources configured, use the manual input
                        if String.isEmpty (String.trim model.utmSource) then
                            []

                        else
                            [ model.utmSource ]

                    else if selectedSourceCount == 0 then
                        -- No sources selected, use defaults or none
                        List.filterMap
                            (\s ->
                                if s.isDefault then
                                    Just s.name

                                else
                                    Nothing
                            )
                            client.sources

                    else
                        Set.toList model.selectedSources

                Nothing ->
                    []

        buttonLabel =
            case List.length sourcesToUse of
                0 ->
                    "Create Short Link"

                1 ->
                    "Create Short Link"

                n ->
                    "Create " ++ String.fromInt n ++ " Short Links"
    in
    Components.Button.primary colors (Just CreateShortLink) buttonLabel


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
                    div [ Attr.class "space-y-4" ]
                        (List.map (viewClientItem model colors) model.clients)
                ]

          else
            text ""
        ]


viewClientItem : FrontendModel -> Theme.Colors -> BitlyClient -> Html FrontendMsg
viewClientItem model colors client =
    let
        isExpanded =
            model.editingClientId == Just client.id
    in
    div
        [ Attr.class "rounded"
        , Attr.style "background-color" colors.inputBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ -- Header row
          div
            [ Attr.class "flex items-center justify-between p-3 cursor-pointer"
            , HE.onClick (SetEditingClient (if isExpanded then Nothing else Just client.id))
            ]
            [ div [ Attr.class "flex items-center gap-2" ]
                [ span
                    [ Attr.style "color" colors.mutedText ]
                    [ text
                        (if isExpanded then
                            "▼"

                         else
                            "▶"
                        )
                    ]
                , span
                    [ Attr.class "font-medium"
                    , Attr.style "color" colors.primaryText
                    ]
                    [ text client.name ]
                , span
                    [ Attr.class "text-xs"
                    , Attr.style "color" colors.mutedText
                    ]
                    [ text
                        ("("
                            ++ String.fromInt (List.length client.sources)
                            ++ " sources, "
                            ++ String.fromInt (List.length client.tags)
                            ++ " tags)"
                        )
                    ]
                ]
            , Components.Button.view
                { variant = Components.Button.Danger
                , size = Components.Button.Small
                , disabled = False
                , onClick = Just (DeleteClient client.id)
                , colors = colors
                }
                "Delete"
            ]

        -- Expanded content
        , if isExpanded then
            div [ Attr.class "p-3 pt-0" ]
                [ -- Sources management
                  div [ Attr.class "mb-4" ]
                    [ label
                        [ Attr.class "block text-sm font-medium mb-2"
                        , Attr.style "color" colors.primaryText
                        ]
                        [ text "Sources" ]
                    , div [ Attr.class "flex flex-wrap gap-2 mb-2" ]
                        (List.map (viewSourceItem colors client.id) client.sources)
                    , div [ Attr.class "flex gap-2" ]
                        [ input
                            [ Attr.type_ "text"
                            , Attr.class "flex-1 p-2 rounded border text-sm"
                            , Attr.style "background-color" colors.secondaryBg
                            , Attr.style "color" colors.primaryText
                            , Attr.style "border-color" colors.border
                            , Attr.placeholder "New source name"
                            , Attr.value model.newSourceInput
                            , HE.onInput NewSourceInputChanged
                            ]
                            []
                        , Components.Button.view
                            { variant = Components.Button.Secondary
                            , size = Components.Button.Small
                            , disabled = False
                            , onClick = Just (AddSourceToClient client.id)
                            , colors = colors
                            }
                            "Add Source"
                        ]
                    ]

                -- Tags management
                , div []
                    [ label
                        [ Attr.class "block text-sm font-medium mb-2"
                        , Attr.style "color" colors.primaryText
                        ]
                        [ text "Tags" ]
                    , div [ Attr.class "flex flex-wrap gap-2 mb-2" ]
                        (List.map (viewTagItem colors client.id) client.tags)
                    , div [ Attr.class "flex gap-2" ]
                        [ input
                            [ Attr.type_ "text"
                            , Attr.class "flex-1 p-2 rounded border text-sm"
                            , Attr.style "background-color" colors.secondaryBg
                            , Attr.style "color" colors.primaryText
                            , Attr.style "border-color" colors.border
                            , Attr.placeholder "New tag"
                            , Attr.value model.newTagInput
                            , HE.onInput NewTagInputChanged
                            ]
                            []
                        , Components.Button.view
                            { variant = Components.Button.Secondary
                            , size = Components.Button.Small
                            , disabled = False
                            , onClick = Just (AddTagToClient client.id)
                            , colors = colors
                            }
                            "Add Tag"
                        ]
                    ]
                ]

          else
            text ""
        ]


viewSourceItem : Theme.Colors -> BitlyClientId -> UtmSource -> Html FrontendMsg
viewSourceItem colors clientId source =
    div
        [ Attr.class "flex items-center gap-1 px-2 py-1 rounded"
        , Attr.style "background-color" colors.secondaryBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ label
            [ Attr.class "flex items-center gap-1 cursor-pointer"
            ]
            [ input
                [ Attr.type_ "checkbox"
                , Attr.checked source.isDefault
                , HE.onClick (ToggleSourceDefault clientId source.name)
                , Attr.title "Set as default"
                ]
                []
            , span
                [ Attr.class "text-sm"
                , Attr.style "color" colors.primaryText
                ]
                [ text source.name ]
            ]
        , button
            [ Attr.class "ml-1 text-xs cursor-pointer hover:opacity-70"
            , Attr.style "color" "#ef4444"
            , HE.onClick (RemoveSourceFromClient clientId source.name)
            ]
            [ text "x" ]
        ]


viewTagItem : Theme.Colors -> BitlyClientId -> String -> Html FrontendMsg
viewTagItem colors clientId tag =
    div
        [ Attr.class "flex items-center gap-1 px-2 py-1 rounded"
        , Attr.style "background-color" colors.secondaryBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ span
            [ Attr.class "text-sm"
            , Attr.style "color" colors.primaryText
            ]
            [ text tag ]
        , button
            [ Attr.class "ml-1 text-xs cursor-pointer hover:opacity-70"
            , Attr.style "color" "#ef4444"
            , HE.onClick (RemoveTagFromClient clientId tag)
            ]
            [ text "x" ]
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
