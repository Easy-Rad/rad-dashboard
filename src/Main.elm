module Main exposing (Msg(..), getXml, init, main, subscriptions, update, view, viewTable, xmlDecoder)

import Browser
import DateFormat
import DateFormat.Relative
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, maximum, minimum, padding, paddingXY, rgb255, rgba255, row, scrollbarX, scrollbarY, scrollbars, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html)
import Html.Parser as Parser
import Html.Parser.Util
import Http
import Iso8601
import Task
import Time
import TimeZone
import Xml.Decode as XD exposing (Decoder, int, list, maybe, oneOf, optionalPath, path, requiredPath, single, string, succeed)
import XmlParser as Xml


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
        -- Modalities
        "ALL"
        -- showNotes
        Nothing
        -- 2021/05/25 12:00
        (Time.millisToPosix 1621943820000)
        -- Timestamp is only available on data arrival
        Nothing
    , Cmd.batch
        [ getXml

        -- , Task.perform Tick Time.now
        ]
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


nz_zone =
    TimeZone.pacific__auckland ()



-- MODEL


type alias Model =
    { studies : List Study
    , modality : String
    , showNotes : Maybe ShowNotesTypes
    , time : Time.Posix
    , dataTimestamp : Maybe Time.Posix

    -- refresh countdown : Int
    }


type alias Study =
    { site : String
    , examType : String
    , description : String
    , nhi : String
    , patientName : String
    , triage : TriageCategory
    , orderDate : String
    , dateReceived : String
    , apptTime : Maybe Time.Posix
    , patientType : String
    , patientLoc : String
    , triageStatus : String
    , generalNotes : String
    , radNotes : String
    }


type alias TriageCategory =
    Int


type alias XmlResult =
    { timeStamp : Time.Posix
    , studies : List Study
    }


locationToUrgency : String -> TriageCategory
locationToUrgency location =
    let
        urgentLocations =
            [ "ED", "CEC", "MAS", "PCU", "SARA", "ICU" ]

        testLocationUrgency urgentLocation =
            String.contains urgentLocation location
    in
    if List.any testLocationUrgency urgentLocations then
        1

    else
        24



-- UPDATE


type Msg
    = Tick Time.Posix
    | GotData (Result Http.Error XmlResult)
    | FilterModality String
    | ShowNotes (Maybe ShowNotesTypes)


type ShowNotesTypes
    = ShowGeneralNotes String
    | ShowRadNotes String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        GotData (Ok xmlResult) ->
            ( { model
                | studies = xmlResult.studies
                , dataTimestamp = Just xmlResult.timeStamp
              }
            , Cmd.none
            )

        FilterModality modality ->
            ( { model | modality = modality }, Cmd.none )

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

        _ ->
            ( -- Debug.log "Unmatched message type"
              model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * 60 * 1000) Tick
        ]



-- VIEW


filterByModality : String -> List Study -> List Study
filterByModality modality studies =
    case modality of
        "ALL" ->
            studies

        _ ->
            List.filter (\study -> study.examType == modality) studies


sortByScore : List Study -> List Study
sortByScore studies =
    studies
        |> List.sortWith
            (by .triage ASC
                |> andThen sortByLocation ASC
                |> andThen sortByWaitingTime ASC
            )


sortByLocation : Study -> Int
sortByLocation study =
    locationToUrgency study.patientLoc


sortByWaitingTime : Study -> Int
sortByWaitingTime study =
    case study.apptTime of
        Just time ->
            Time.posixToMillis time

        Nothing ->
            0


type Direction
    = ASC
    | DESC


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


view : Model -> Html Msg
view model =
    let
        studies =
            model.studies |> filterByModality model.modality |> sortByScore

        viewModalityButton modality label =
            let
                selected =
                    model.modality == modality

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
                { onPress = Just <| FilterModality modality, label = text label }
    in
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
                [ viewModalityButton "ALL" "All"
                , viewModalityButton "XR" "XR"
                , viewModalityButton "CT" "CT"
                , viewModalityButton "MR" "MR"
                ]
            , viewTable model.time studies
            , column [ centerX, padding 10, Font.size 12, spacing 5 ]
                [ el [ Font.center, centerX ] (text " Christchurch Hospital")
                , el [ Font.center, centerX ] (text "Department of Radiology")
                ]
            ]
        )


viewTimeInfo : Model -> Element Msg
viewTimeInfo model =
    let
        dataTimestamp =
            case model.dataTimestamp of
                Just time ->
                    dateFormatter nz_zone time

                Nothing ->
                    "N/A"

        currentTimestamp =
            dateFormatter nz_zone model.time

        viewTime =
            column [ Font.family [ Font.monospace ], alignRight, Font.size 12 ]
                [ row [ alignRight ] [ text "Last refresh: ", el [] (text <| dataTimestamp) ]
                , row [ alignRight ] [ text "Current time: ", el [] (text <| currentTimestamp) ]
                ]
    in
    el [ Element.inFront viewTime, alignTop ] Element.none


viewNotesOverlay model =
    let
        parseNotes content =
            case Parser.run content of
                Ok nodes ->
                    el
                        [ alignBottom
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
                            Element.html
                                (Html.node "div"
                                    []
                                    (Html.Parser.Util.toVirtualDom nodes)
                                )

                Err _ ->
                    el [] (text "Malformed notes, please see COMRAD.")
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


viewTable : Time.Posix -> List Study -> Element Msg
viewTable time studies =
    case studies of
        [] ->
            viewEmptyTable

        _ ->
            Element.table
                [ padding 5
                , width fill
                , centerX
                , Border.color <| rgb255 111 195 223
                , Border.width 2
                , Border.rounded 5
                ]
                { data = studies
                , columns =
                    [ { header = el [ headerBorder, headerPadding, Font.center ] <| text "Modality"
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
                                    text study.examType
                      }
                    , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Name"
                      , width = fillPortion 2
                      , view =
                            \study ->
                                el
                                    [ rowPadding
                                    , Font.center
                                    ]
                                <|
                                    text study.patientName
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
                                case study.apptTime of
                                    Just referralTime ->
                                        el
                                            [ rowPadding
                                            , Font.center
                                            , Font.family [ Font.monospace ]
                                            , Font.extraLight
                                            , Font.size 16
                                            ]
                                        <|
                                            text (dateFormatter nz_zone referralTime)

                                    Nothing ->
                                        el [ rowPadding, Font.center, Font.size 16 ] <| text ""
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
                                        triageCategoryToString study.triage
                      }
                    , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Location"
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
                                    text study.patientLoc
                      }
                    , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Due in"
                      , width = fill
                      , view =
                            \study ->
                                case dueInTime study of
                                    Just due ->
                                        let
                                            diff =
                                                (Time.posixToMillis due - Time.posixToMillis time) // (1000 * 60 * 60)

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
                                            text (DateFormat.Relative.relativeTime time due)

                                    Nothing ->
                                        el
                                            [ rowPadding
                                            , Font.center
                                            ]
                                        <|
                                            text "-"
                      }
                    ]
                }


viewEmptyTable : Element Msg
viewEmptyTable =
    el
        [ width fill
        , height fill
        , Border.width 2
        , Border.color <| rgb255 111 195 223
        , Border.rounded 5
        , padding 5
        ]
    <|
        el [ centerX, centerY, Font.center ] (text "Loading...")


viewNotesIcons : String -> String -> Element Msg
viewNotesIcons generalNotes radNotes =
    let
        viewGeneralNotes : String -> Element Msg
        viewGeneralNotes content =
            case content of
                "" ->
                    Element.none

                _ ->
                    el
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

        viewRadNotes : String -> Element Msg
        viewRadNotes content =
            case content of
                "" ->
                    Element.none

                _ ->
                    el
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
    case ( generalNotes, radNotes ) of
        ( "", "" ) ->
            Element.none

        _ ->
            row [ width (fillPortion 1), spacing 10 ]
                [ viewGeneralNotes generalNotes
                , viewRadNotes radNotes
                ]


dueInTime : Study -> Maybe Time.Posix
dueInTime study =
    case study.apptTime of
        Just referralTime ->
            let
                triageHour =
                    study.triage

                referralTimeInPosix =
                    Time.posixToMillis referralTime

                triageBasedDue =
                    referralTimeInPosix + triageHour * 60 * 60 * 1000

                locationBasedDue =
                    referralTimeInPosix + locationToUrgency study.patientLoc * 60 * 60 * 1000
            in
            if triageHour >= 999 then
                -- Ignore not triaged, planned etc.
                Nothing

            else if triageBasedDue >= locationBasedDue then
                Just (Time.millisToPosix locationBasedDue)

            else
                Just (Time.millisToPosix triageBasedDue)

        Nothing ->
            Nothing



-- HTTP


liveUrl : String
liveUrl =
    "http://159.117.39.240/apps/monitorworklistdisplay-ereferrals-summary.xml"


getXml : Cmd Msg
getXml =
    Http.get
        { url = "anonymised_summary.xml"
        , expect = expectXml GotData xmlDecoder
        }


expectXml : (Result Http.Error a -> msg) -> XD.Decoder a -> Http.Expect msg
expectXml toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case XD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody err)


xmlDecoder : Decoder XmlResult
xmlDecoder =
    succeed XmlResult
        |> requiredPath [ "extractdatetime" ] (single timeDecoder)
        |> requiredPath [ "ereferral", "study" ] (list studyDecoder)


triageCategoryToString : TriageCategory -> String
triageCategoryToString category =
    case category of
        0 ->
            "STAT"

        1 ->
            "1 hour"

        4 ->
            "4 hours"

        24 ->
            "24 hours"

        48 ->
            "2 days"

        336 ->
            "2 weeks"

        999 ->
            "Planned"

        9999 ->
            "Not triaged"

        _ ->
            "-"


studyDecoder : Decoder Study
studyDecoder =
    succeed Study
        |> requiredPath [ "site" ] (single string)
        |> requiredPath [ "examtype" ] (single string)
        |> requiredPath [ "description" ] (single string)
        |> requiredPath [ "NHI" ] (single string)
        |> requiredPath [ "patientname" ] (single string)
        |> requiredPath [ "status" ] (single triageCategoryDecoder)
        |> requiredPath [ "orderdate" ] (single string)
        |> requiredPath [ "datereceived" ] (single string)
        |> requiredPath [ "appttime" ] (single <| maybe timeDecoder)
        |> requiredPath [ "pattype" ] (single string)
        |> requiredPath [ "patloc" ] (single string)
        |> requiredPath [ "triagestatus" ] (single string)
        |> optionalPath [ "notes", "html", "body" ] (single embedNodeDecoder) ""
        |> optionalPath [ "radnotes", "html", "body" ] (single embedNodeDecoder) ""


triageCategoryDecoder : Decoder TriageCategory
triageCategoryDecoder =
    let
        parseTriageCategory triage =
            case triage of
                "STAT" ->
                    0

                "1 HOUR" ->
                    1

                "4 HOURS" ->
                    4

                "24 HOURS" ->
                    24

                "2 DAYS" ->
                    48

                "2 WEEKS" ->
                    336

                "PLANNED" ->
                    999

                "NOTTRIAGED" ->
                    9999

                _ ->
                    -- Debug.log ("Unknown triage category (status): " ++ triage)
                    99999
    in
    XD.map parseTriageCategory string


timeDecoder : Decoder Time.Posix
timeDecoder =
    XD.string
        |> XD.andThen
            (\str ->
                let
                    iso =
                        String.replace " " "T" str ++ "Z"
                in
                case Iso8601.toTime iso of
                    Err _ ->
                        XD.fail "Time decoding failed"

                    Ok time ->
                        XD.succeed time
            )


embedNodeDecoder : Decoder String
embedNodeDecoder =
    let
        nodeToString node =
            Xml.Xml [] Nothing node
                |> Xml.format
    in
    XD.map nodeToString XD.node



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
