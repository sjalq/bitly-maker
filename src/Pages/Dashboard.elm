module Pages.Dashboard exposing (init, view)

import Components.Button
import Components.Card
import Components.Header
import Dict
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as HE
import Theme
import Time
import Types exposing (..)


init : FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
init model =
    ( model, Cmd.none )


view : FrontendModel -> Theme.Colors -> Html FrontendMsg
view model colors =
    div [ Attr.style "background-color" colors.primaryBg, Attr.class "min-h-screen" ]
        [ div [ Attr.class "container mx-auto px-4 md:px-6 py-4 md:py-8" ]
            [ Components.Header.pageHeader colors "Links Dashboard" (Just "View and manage all your created links")
            , case model.login of
                LoggedIn _ ->
                    viewDashboard model colors

                _ ->
                    viewLoginPrompt colors
            ]
        ]


viewLoginPrompt : Theme.Colors -> Html FrontendMsg
viewLoginPrompt colors =
    div [ Attr.class "mt-8" ]
        [ Components.Card.simple colors
            [ p [ Attr.class "text-center", Attr.style "color" colors.primaryText ]
                [ text "Please log in to view your links dashboard." ]
            , div [ Attr.class "mt-4 text-center" ]
                [ Components.Button.primary colors (Just ToggleLoginModal) "Log In" ]
            ]
        ]


viewDashboard : FrontendModel -> Theme.Colors -> Html FrontendMsg
viewDashboard model colors =
    let
        -- Get all links from frontend model
        links =
            model.linksForDashboard

        -- Apply filters
        filteredLinks =
            applyFilters model.dashboardFilters links

        -- Apply sorting
        sortedLinks =
            applySort model.dashboardSortColumn model.dashboardSortDirection filteredLinks
    in
    div [ Attr.class "mt-6" ]
        [ -- Filters row
          viewFilters model colors

        -- Links table
        , if List.isEmpty links then
            Components.Card.simple colors
                [ p [ Attr.class "text-center py-8", Attr.style "color" colors.mutedText ]
                    [ text "No links created yet. Create some links on the Home page!" ]
                ]

          else
            viewLinksTable sortedLinks model colors
        ]


viewFilters : FrontendModel -> Theme.Colors -> Html FrontendMsg
viewFilters model colors =
    div
        [ Attr.class "mb-4 p-4 rounded"
        , Attr.style "background-color" colors.secondaryBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ div [ Attr.class "flex flex-wrap gap-3 items-end" ]
            [ viewFilterInput colors "Source" "source" model.dashboardFilters
            , viewFilterInput colors "Medium" "medium" model.dashboardFilters
            , viewFilterInput colors "Campaign" "campaign" model.dashboardFilters
            , viewFilterInput colors "Tags" "tags" model.dashboardFilters
            , Components.Button.secondary colors (Just ClearDashboardFilters) "Clear Filters"
            ]
        ]


viewFilterInput : Theme.Colors -> String -> String -> Dict.Dict String String -> Html FrontendMsg
viewFilterInput colors labelText filterKey filters =
    div [ Attr.class "flex-1 min-w-32" ]
        [ label
            [ Attr.class "block text-xs font-medium mb-1"
            , Attr.style "color" colors.mutedText
            ]
            [ text labelText ]
        , input
            [ Attr.type_ "text"
            , Attr.class "w-full p-2 rounded border text-sm"
            , Attr.style "background-color" colors.inputBg
            , Attr.style "color" colors.primaryText
            , Attr.style "border-color" colors.border
            , Attr.placeholder ("Filter by " ++ String.toLower labelText)
            , Attr.value (Dict.get filterKey filters |> Maybe.withDefault "")
            , HE.onInput (SetDashboardFilter filterKey)
            ]
            []
        ]


viewLinksTable : List CreatedLink -> FrontendModel -> Theme.Colors -> Html FrontendMsg
viewLinksTable links model colors =
    div
        [ Attr.class "overflow-x-auto rounded"
        , Attr.style "background-color" colors.secondaryBg
        , Attr.style "border" ("1px solid " ++ colors.border)
        ]
        [ table [ Attr.class "w-full" ]
            [ thead []
                [ tr [ Attr.style "border-bottom" ("1px solid " ++ colors.border) ]
                    [ viewSortableHeader colors "Created" ColCreatedAt model.dashboardSortColumn model.dashboardSortDirection
                    , viewSortableHeader colors "Destination" ColDestinationUrl model.dashboardSortColumn model.dashboardSortDirection
                    , viewSortableHeader colors "Source" ColSource model.dashboardSortColumn model.dashboardSortDirection
                    , viewSortableHeader colors "Medium" ColMedium model.dashboardSortColumn model.dashboardSortDirection
                    , viewSortableHeader colors "Campaign" ColCampaign model.dashboardSortColumn model.dashboardSortDirection
                    , viewSortableHeader colors "Short URL" ColShortUrl model.dashboardSortColumn model.dashboardSortDirection
                    , viewSortableHeader colors "Tags" ColTags model.dashboardSortColumn model.dashboardSortDirection
                    ]
                ]
            , tbody []
                (List.map (viewLinkRow colors) links)
            ]
        ]


viewSortableHeader : Theme.Colors -> String -> DashboardColumn -> DashboardColumn -> SortDirection -> Html FrontendMsg
viewSortableHeader colors label column currentColumn currentDirection =
    let
        isActive =
            column == currentColumn

        arrow =
            if isActive then
                case currentDirection of
                    Ascending ->
                        " ↑"

                    Descending ->
                        " ↓"

            else
                ""
    in
    th
        [ Attr.class "px-4 py-3 text-left text-sm font-medium cursor-pointer select-none"
        , Attr.style "color" colors.primaryText
        , Attr.style "background-color" colors.inputBg
        , HE.onClick (SetDashboardSort column)
        ]
        [ text (label ++ arrow) ]


viewLinkRow : Theme.Colors -> CreatedLink -> Html FrontendMsg
viewLinkRow colors link =
    tr
        [ Attr.style "border-bottom" ("1px solid " ++ colors.border)
        , Attr.class "hover:opacity-80"
        ]
        [ td [ Attr.class "px-4 py-3 text-sm", Attr.style "color" colors.mutedText ]
            [ text (formatTime link.createdAt) ]
        , td [ Attr.class "px-4 py-3 text-sm max-w-48 truncate", Attr.style "color" colors.primaryText ]
            [ text link.destinationUrl ]
        , td [ Attr.class "px-4 py-3 text-sm", Attr.style "color" colors.primaryText ]
            [ text link.utmSource ]
        , td [ Attr.class "px-4 py-3 text-sm", Attr.style "color" colors.primaryText ]
            [ text link.utmMedium ]
        , td [ Attr.class "px-4 py-3 text-sm", Attr.style "color" colors.primaryText ]
            [ text link.utmCampaign ]
        , td [ Attr.class "px-4 py-3 text-sm font-mono", Attr.style "color" "#4ade80" ]
            [ span
                [ Attr.class "cursor-pointer"
                , HE.onClick (CopyToClipboard link.shortUrl)
                ]
                [ text (String.replace "https://" "" link.shortUrl) ]
            ]
        , td [ Attr.class "px-4 py-3 text-sm", Attr.style "color" colors.mutedText ]
            [ div [ Attr.class "flex flex-wrap gap-1" ]
                (List.map (viewTag colors) link.tags)
            ]
        ]


viewTag : Theme.Colors -> String -> Html msg
viewTag colors tag =
    span
        [ Attr.class "px-2 py-1 rounded text-xs"
        , Attr.style "background-color" colors.inputBg
        , Attr.style "color" colors.primaryText
        ]
        [ text tag ]


formatTime : Time.Posix -> String
formatTime posix =
    let
        millis =
            Time.posixToMillis posix

        -- Simple formatting: just show the date
        -- For a real app, you'd want a proper date formatting library
        days =
            millis // (1000 * 60 * 60 * 24)

        -- Days since epoch
        -- Simple approximation of date
        year =
            1970 + (days // 365)

        dayOfYear =
            modBy 365 days

        month =
            dayOfYear // 30 + 1

        day =
            modBy 30 dayOfYear + 1
    in
    String.fromInt year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt month)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt day)



-- Helper functions


applyFilters : Dict.Dict String String -> List CreatedLink -> List CreatedLink
applyFilters filters links =
    let
        sourceFilter =
            Dict.get "source" filters |> Maybe.withDefault "" |> String.toLower

        mediumFilter =
            Dict.get "medium" filters |> Maybe.withDefault "" |> String.toLower

        campaignFilter =
            Dict.get "campaign" filters |> Maybe.withDefault "" |> String.toLower

        tagsFilter =
            Dict.get "tags" filters |> Maybe.withDefault "" |> String.toLower

        matchesFilter filterValue linkValue =
            String.isEmpty filterValue || String.contains filterValue (String.toLower linkValue)

        matchesTags filterValue tags =
            String.isEmpty filterValue || List.any (\tag -> String.contains filterValue (String.toLower tag)) tags
    in
    links
        |> List.filter
            (\link ->
                matchesFilter sourceFilter link.utmSource
                    && matchesFilter mediumFilter link.utmMedium
                    && matchesFilter campaignFilter link.utmCampaign
                    && matchesTags tagsFilter link.tags
            )


applySort : DashboardColumn -> SortDirection -> List CreatedLink -> List CreatedLink
applySort column direction links =
    let
        comparator =
            case column of
                ColCreatedAt ->
                    \a b -> compare (Time.posixToMillis a.createdAt) (Time.posixToMillis b.createdAt)

                ColDestinationUrl ->
                    \a b -> compare a.destinationUrl b.destinationUrl

                ColSource ->
                    \a b -> compare a.utmSource b.utmSource

                ColMedium ->
                    \a b -> compare a.utmMedium b.utmMedium

                ColCampaign ->
                    \a b -> compare a.utmCampaign b.utmCampaign

                ColShortUrl ->
                    \a b -> compare a.shortUrl b.shortUrl

                ColTags ->
                    \a b -> compare (String.join "," a.tags) (String.join "," b.tags)

        sorted =
            List.sortWith comparator links
    in
    case direction of
        Ascending ->
            sorted

        Descending ->
            List.reverse sorted
