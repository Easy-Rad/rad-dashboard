module Main exposing (Msg(..), fetchData, init, main, update, view, viewTable)

import Browser
import DateFormat
import DateFormat.Relative
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, column, el, fill, fillPortion, height, maximum, padding, paddingXY, rgb255, rgba255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http exposing (Error(..))
import Task
import Time
import TimeZone
import Http exposing (expectJson)
import Json.Decode exposing (Decoder, succeed, string, map, list, int)
import Json.Decode.Pipeline exposing (optional, required)
import Browser
import Element exposing (textColumn)
import Browser exposing (Document)
import Html.Attributes exposing (title)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Ok [])
        CT
        -- showNotes
        Nothing
        Nothing
    , 
    Task.perform FetchData Time.now 
    )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


nz_zone : Time.Zone
nz_zone =
    TimeZone.pacific__auckland ()



-- MODEL


type alias Model =
    { result : Result Http.Error (List Study)
    , modality : Modality
    , showNotes : Maybe ShowNotesTypes
    , dataTimestamp : Maybe Time.Posix
    }


type alias Study =
    { site : String
    , description : String
    , nhi : String
    , pa_firstname : String
    , pa_surname : String
    , pa_type : String
    , urgency : TriageCategory
    , received: Time.Posix
    , location : Maybe String
    , generalNotes : List String
    , radNotes : List String
    }

type Modality =
    CT | DSA | MR | NM | US| XR

type alias TriageCategory =
    Int


locationToUrgency : Maybe String -> TriageCategory
locationToUrgency location =
    let
        urgentLocations =
            [ "ED", "CEC", "MAS", "PCU", "SARA", "ICU" ]

    in
    case location of
        Just l ->
            if List.any (\u -> String.contains u l) urgentLocations then
                1
            else
                24
        Nothing ->
            24



-- UPDATE


type Msg
    = FetchData Time.Posix
    | GotResult Time.Posix (Result Http.Error (List Study))
    | ChangeModality Modality
    | ShowNotes (Maybe ShowNotesTypes)


type ShowNotesTypes
    = ShowGeneralNotes (List String)
    | ShowRadNotes (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchData timestamp ->
            ( model, fetchData timestamp model.modality )

        GotResult timestamp result ->
            ( { model
                | result = result
                , dataTimestamp = Just timestamp
              }
            , Cmd.none
            )
        
        ChangeModality modality ->
            ( { model | modality = modality }, Task.perform FetchData Time.now )

        ShowNotes notesTypes ->
            case notesTypes of
                Just (ShowGeneralNotes content) ->
                    ( { model | showNotes = Just (ShowGeneralNotes content) }
                    , Cmd.none
                    )

                Just (ShowRadNotes content) ->
                    ( { model | showNotes = Just (ShowRadNotes content) }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | showNotes = Nothing }
                    , Cmd.none
                    )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (60*1000) FetchData

-- VIEW

sortByScore : List Study -> List Study
sortByScore studies =
    studies
        |> List.sortWith
            (by .urgency ASC
                |> andThen sortByLocation ASC
                |> andThen sortByWaitingTime ASC
            )


sortByLocation : Study -> Int
sortByLocation study =
    locationToUrgency study.location


sortByWaitingTime : Study -> Int
sortByWaitingTime study =
    Time.posixToMillis study.received


type Direction
    = ASC | DESC


by : (a -> comparable) -> Direction -> (a -> a -> Order)
by toCmp direction a b =
    case ( compare (toCmp a) (toCmp b), direction ) of
        ( LT, ASC ) ->
            LT

        ( LT, DESC ) ->
            GT

        ( GT, ASC ) ->
            GT

        ( GT, DESC ) ->
            LT

        ( EQ, _ ) ->
            EQ


andThen : (a -> comparable) -> Direction -> (a -> a -> Order) -> (a -> a -> Order)
andThen toCmp direction primary a b =
    case primary a b of
        EQ ->
            by toCmp direction a b

        ineq ->
            ineq


view : Model -> Document Msg
view model = 
    Element.layout
        [ Background.color <| rgb255 12 20 31
        , Font.color <| rgb255 230 255 255
        , Font.family [ Font.typeface "Roboto", Font.sansSerif ]
        , width fill
        , Element.inFront (viewNotesOverlay model)
        , Events.onMouseUp (ShowNotes Nothing)
        ]
        (column
            [ padding 10
            , centerX
            , width fill
            , height fill
            , Element.scrollbars
            ]
            [ row
                [ padding 10
                , Font.size 30
                , Font.family [ Font.typeface "Orbitron", Font.sansSerif ]
                , width fill
                ]
                [ el
                    [ centerX
                    , Element.onRight
                        (el [ Font.family [ Font.sansSerif ], Font.size 12, alignTop, paddingXY 3 0 ]
                            (text "alpha")
                        )
                    ]
                    (text "Radiology Dashboard")
                , viewTimeInfo model
                ]
            , row [ padding 30, spacing 20, centerX ]
                [ viewModalityButton model.modality XR
                , viewModalityButton model.modality CT
                , viewModalityButton model.modality MR
                , viewModalityButton model.modality US
                , viewModalityButton model.modality NM
                , viewModalityButton model.modality DSA
                ]
            , case model.result of
                Ok studies ->
                    studies |> sortByScore |> viewTable model.dataTimestamp
                Err err ->
                    viewError err
            , column [ centerX, padding 10, Font.size 12, spacing 5 ]
                [ el [ Font.center, centerX ] (text " Christchurch Hospital")
                , el [ Font.center, centerX ] (text "Department of Radiology")
                ]
            ]
        )
        |> List.singleton |> Document "Radiology Dashboard"


viewModalityButton : Modality -> Modality -> Element Msg
viewModalityButton current modality =
    let
        selected =
            current == modality

        selectedStyle =
            [ Background.color (rgb255 255 255 255)
            , Font.color (rgb255 0 0 0)
            ]
    in
    Input.button
        ([ Border.width 1
         , Border.rounded 5
         , Border.glow (rgb255 255 255 255) 0.5
         , padding 5
         ]
            ++ (if selected then
                    selectedStyle

                else
                    []
               )
        )
        { onPress = Just <| ChangeModality modality
        , label = text <| case modality of 
            XR -> "XR"
            CT -> "CT"
            MR -> "MR"
            DSA -> "DSA"
            NM -> "NM"
            US -> "US"
         }


viewTimeInfo : Model -> Element Msg
viewTimeInfo model =
    let
        dataTimestamp =
            case model.dataTimestamp of
                Just time ->
                    dateFormatter nz_zone time

                Nothing ->
                    "N/A"

        viewTime =
            column [ Font.family [ Font.monospace ], alignRight, Font.size 12 ]
                [ row [ alignRight ] [ text "Last refresh: ", el [] (text <| dataTimestamp) ]
                -- , row [ alignRight ] [ text "Current time: ", el [] (text <| currentTimestamp) ]
                ]
    in
    el [ Element.inFront viewTime, alignTop ] Element.none


viewNotesOverlay : { a | showNotes : Maybe ShowNotesTypes } -> Element Msg
viewNotesOverlay model =
    let
        parseNotes: List String -> Element Msg
        parseNotes content =

            el[ alignBottom
            , padding 20
            , width fill
            , height (shrink |> maximum 500)
            , Element.scrollbars
            , Background.color (rgb255 255 255 255)
            , Font.size 16
            , Font.color (rgb255 0 0 0)
            , Element.inFront
                (el
                    [ alignTop
                    , alignRight
                    , padding 1
                    , Events.onMouseDown (ShowNotes Nothing)
                    , Element.pointer
                    , width (Element.px 20)
                    , height (Element.px 20)
                    , Font.center
                    ]
                    (text "x")
                )
            ]
            <|
            el [ centerX, padding 5 ] <|
            textColumn[] <| List.map (\note -> el[] <| text note) <| content
                        
    in
    case model.showNotes of
        Just (ShowGeneralNotes content) ->
            parseNotes content

        Just (ShowRadNotes content) ->
            parseNotes content

        Nothing ->
            Element.none


dateFormatter : Time.Zone -> Time.Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.dayOfMonthFixed
        , DateFormat.text "/"
        , DateFormat.monthNumber
        , DateFormat.text "/"
        , DateFormat.yearNumberLastTwo
        , DateFormat.text " "
        , DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        ]

viewError : Http.Error -> Element Msg
viewError err =
    (case err of
        BadBody b ->
            b

        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "Request timed out."

        NetworkError ->
            "Network error."

        BadStatus status ->
            "Bad status: " ++ String.fromInt status
    )
    |> text
    |> el
        [ Font.size 12
        , Font.family [ Font.monospace ]
        ]

viewTable : Maybe Time.Posix -> List Study -> Element Msg
viewTable time studies =
    el [ width shrink ] <|
        Element.table
            [ padding 5
            , centerX

            -- , width shrink
            , Border.color <| rgb255 111 195 223
            , Border.width 2
            , Border.rounded 5
            ]
            { data = studies
            , columns =
                [ { header = el [ headerBorder, headerPadding, Font.alignLeft ] <| text "Name"
                    , width = fillPortion 2
                    , view =
                        \study ->
                            el
                                [ rowPadding
                                , Font.alignLeft
                                ]
                            <| text <| study.pa_surname ++ ", " ++ study.pa_firstname
                    }
                , { header = el [ headerBorder, headerPadding, Font.center ] <| text "NHI"
                    , width = fill
                    , view =
                        \study ->
                            el
                                [ rowPadding
                                , Font.center
                                , Font.family [ Font.monospace ]
                                , Font.size 16
                                ]
                            <|
                                el
                                    [ Background.color (rgb255 223 116 12)
                                    , Border.rounded 5
                                    , Font.color (rgb255 0 0 0)
                                    , centerX
                                    , padding 3
                                    ]
                                    (text study.nhi)
                    }
                , { header = el [ headerBorder, headerPadding, Font.alignLeft ] <| text "Examination"
                    , width = fillPortion 4
                    , view =
                        \study ->
                            row [ rowPadding, spacing 5, Font.size 16 ]
                                [ Element.paragraph
                                    [ Font.alignLeft
                                    , Font.extraBold
                                    , width (fillPortion 18)
                                    ]
                                    [ text study.description ]
                                , viewNotesIcons study.generalNotes study.radNotes
                                ]
                    }
                , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Referral Time"
                    , width = fillPortion 2
                    , view =
                        \study ->
                            el
                                [ rowPadding
                                , Font.center
                                , Font.family [ Font.monospace ]
                                , Font.extraLight
                                , Font.size 16
                                ]
                            <|
                                text (dateFormatter nz_zone study.received)
                    }
                , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Triage Category"
                    , width = fill
                    , view =
                        \study ->
                            el
                                [ rowPadding
                                , Font.color <| rgb255 223 116 12
                                , Font.center
                                , Font.family [ Font.monospace ]
                                , Font.size 16
                                ]
                            <|
                                text <|
                                    triageCategoryToString study.urgency
                    }
                , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Location"
                    , width = fill
                    , view =
                        \study ->
                            case study.location of
                                Just location ->
                                    el
                                        [ rowPadding
                                        , Font.center
                                        , Font.family [ Font.monospace ]
                                        , Font.size 16
                                        ]
                                    <|
                                        text location
                                Nothing -> Element.none
                    }
                , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Due"
                    , width = fill
                    , view =
                        \study ->
                            case (dueInTime study, time) of
                                (Just due, Just t) ->
                                    let
                                        diff =
                                            (Time.posixToMillis due - Time.posixToMillis t) // (1000 * 60 * 60)

                                        alpha =
                                            if diff < 0 then
                                                1.0

                                            else if diff <= 4 then
                                                1 - (toFloat diff / 4)

                                            else
                                                0.0
                                    in
                                    el
                                        [ rowPadding
                                        , height fill
                                        , Font.center
                                        , Font.size 18

                                        -- , height fill
                                        , Background.color (rgba255 255 0 0 alpha)
                                        ]
                                    <|
                                        text (DateFormat.Relative.relativeTime t due)
                                        -- text (Time.posixToMillis due |> fromInt)

                                _ ->
                                    el
                                        [ rowPadding
                                        , Font.center
                                        ]
                                    <|
                                        text "-"
                    }
                ]
            }



viewNotesIcons : List String -> List String -> Element Msg
viewNotesIcons generalNotes radNotes =
    let
        viewGeneralNotes : List String -> Element Msg
        viewGeneralNotes content =
            if List.isEmpty content then Element.none
            else el
                [ Background.color (rgb255 0 0 255)
                , Font.center
                , Font.bold
                , Font.family [ Font.serif ]
                , Border.rounded 10
                , alignRight
                , padding 2
                , width (Element.px 20)
                , height (Element.px 20)
                , Element.pointer
                , Events.onClick (ShowNotes <| Just (ShowGeneralNotes content))
                ]
                (text "i")

        viewRadNotes : List String -> Element Msg
        viewRadNotes content =
            if List.isEmpty content then Element.none
            else el
                    [ Background.color (rgb255 255 255 0)
                    , Font.center
                    , Font.bold
                    , Font.color (rgb255 0 0 0)
                    , Border.rounded 10
                    , alignRight
                    , padding 2
                    , width (Element.px 20)
                    , height (Element.px 20)
                    , Element.pointer
                    , Events.onClick (ShowNotes <| Just (ShowRadNotes content))
                    ]
                    (text "R")
    in
    if List.isEmpty generalNotes && List.isEmpty radNotes then Element.none
    else row [ width (fillPortion 1), spacing 10 ]
        [ viewGeneralNotes generalNotes
        , viewRadNotes radNotes
        ]


dueInTime : Study -> Maybe Time.Posix
dueInTime study =
    let
        triageHour =
            study.urgency

        referralTimeInPosix =
            Time.posixToMillis study.received

        triageBasedDue =
            referralTimeInPosix + triageHour * 60 * 60 * 1000

        locationBasedDue =
            referralTimeInPosix + locationToUrgency study.location * 60 * 60 * 1000
    in
    if triageHour >= 1009 then
        -- Ignore not triaged, planned etc.
        Nothing

    else if triageBasedDue >= locationBasedDue then
        Just (Time.millisToPosix locationBasedDue)

    else
        Just (Time.millisToPosix triageBasedDue)


-- HTTP


fetchData : Time.Posix -> Modality -> Cmd Msg
fetchData timestamp modality =
    Http.get
        { url = "https://api.easyrad.duckdns.org/dashboard/" ++ case modality of 
                XR -> "XR"
                CT -> "CT"
                MR -> "MR"
                DSA -> "DS"
                NM -> "NM"
                US -> "US"
        , expect = expectJson (GotResult timestamp) (list studyDecoder)
        }


triageCategoryToString : TriageCategory -> String
triageCategoryToString category =
    case category of
        0 -> "STAT"
        1 -> "1 hour"
        4 -> "4 hours"
        24 -> "24 hours"
        48 -> "2 days"
        336 -> "2 weeks"
        672 -> "4 weeks"
        1008 -> "6 weeks"
        1009 -> "DAROT"
        1010 -> "Planned"
        9999 -> "Not triaged"
        _ -> "-"


studyDecoder : Decoder Study
studyDecoder =
    succeed Study
        |> required "site" string
        |> required "description" string
        |> required "nhi" string
        |> required "pa_firstname" string
        |> required "pa_surname" string
        |> required "patient_type" string
        |> optional "urgency" triageCategoryDecoder 9999
        |> required "received" posixDecoder
        |> optional "location" (map Just string) Nothing
        |> required "gen_notes" (list string)
        |> required "rad_notes" (list string)

triageCategoryDecoder : Decoder TriageCategory
triageCategoryDecoder =
    let
        parseTriageCategory triage =
            case triage of
                "STAT" -> 0
                "1 hour" -> 1
                "4 hours" -> 4
                "24 hours" -> 24
                "2 days" -> 48
                "2 weeks" -> 336
                "4 weeks" -> 672
                "6 weeks" -> 1008
                "DAROT" -> 1009
                "Planned" -> 1010
                _ -> 99999
    in
    map parseTriageCategory string


posixDecoder : Decoder Time.Posix
posixDecoder =
    map (\i -> Time.millisToPosix (i*1000)) int


-- Styles


headerBorder : Element.Attribute Msg
headerBorder =
    Border.widthEach
        { bottom = 2
        , left = 0
        , right = 0
        , top = 0
        }


headerPadding : Element.Attribute Msg
headerPadding =
    Element.paddingEach { top = 10, right = 5, left = 5, bottom = 15 }


rowPadding : Element.Attribute Msg
rowPadding =
    Element.paddingEach { top = 15, right = 5, left = 5, bottom = 15 }
