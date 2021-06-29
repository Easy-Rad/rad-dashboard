module Main exposing (Msg(..), getXml, init, main, subscriptions, update, view, viewTable, xmlDecoder)

import Browser
import Date exposing (Date)
import DateFormat
import DateFormat.Relative
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, maximum, minimum, padding, paddingXY, rgb255, rgba255, row, scrollbarX, scrollbarY, scrollbars, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Html exposing (Attribute, Html)
import Http
import Iso8601
import Task
import Time
import TimeZone
import Xml.Decode as Decode exposing (Decoder, int, list, maybe, oneOf, path, requiredPath, run, single, string, succeed)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
        "ALL"
        -- 2021/05/25 12:00
        (Time.millisToPosix 1621942200000)
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
    , time : Time.Posix

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
    , patientType : String -- todo: prob not v useful
    , patientLoc : String
    , triageStatus : String
    }


type alias TriageCategory =
    Int


locationToUrgency : String -> TriageCategory
locationToUrgency location =
    let
        urgentLocations =
            -- todo: change to regex matching
            [ "ED", "CEC", "MAS", "A3SPCU", "B3SARA", "ICUH" ]
    in
    if List.member location urgentLocations then
        1

    else
        24



-- UPDATE


type Msg
    = GotData (Result Http.Error String)
    | FilterModality String
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok rawXmlString) ->
            case run xmlDecoder rawXmlString of
                Ok referrals ->
                    ( { model | studies = referrals }, Cmd.none )

                Err error ->
                    ( -- Debug.log ("Error" ++ error)
                      { model | studies = [], modality = "all" }
                    , Cmd.none
                    )

        FilterModality modality ->
            ( { model | modality = modality }, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

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
    studies |> List.sortWith (by .triage ASC |> andThen patientTypeScore ASC)


patientTypeScore : Study -> Int
patientTypeScore study =
    case study.patientType of
        "ED" ->
            0

        "INP" ->
            1

        _ ->
            9


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
    in
    Element.layout
        [ Background.color <| rgb255 12 20 31
        , Font.color <| rgb255 230 255 255
        , Font.family [ Font.typeface "Roboto", Font.sansSerif ]
        , width fill
        , padding 10
        ]
        (column
            [ width fill
            , height fill
            , padding 10
            ]
            [ row
                [ padding 20
                , Font.size 30
                , Font.family [ Font.typeface "Orbitron", Font.sansSerif ]
                , centerX
                ]
                [ text "Radiology Dashboard"
                , el [ Font.family [ Font.sansSerif ], Font.size 12, alignTop, paddingXY 3 0 ] (text "alpha")
                ]
            , el [ Font.center, centerX ] (text (dateFormatter nz_zone model.time))
            , row [ padding 30, spacing 20, centerX ]
                [ Input.button [] { onPress = Just <| FilterModality "ALL", label = text "All" }
                , Input.button [] { onPress = Just <| FilterModality "XR", label = text "XR" }
                , Input.button [] { onPress = Just <| FilterModality "CT", label = text "CT" }
                , Input.button [] { onPress = Just <| FilterModality "MR", label = text "MR" }
                ]
            , el
                [ padding 5
                , width fill
                , height fill
                , Element.scrollbarX
                , Border.color <| rgb255 111 195 223
                , Border.width 2
                , Border.rounded 0
                ]
                (viewTable model.time studies)
            ]
        )


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


dateFormatter : Time.Zone -> Time.Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
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
    Element.table
        []
        { data = studies
        , columns =
            [ { header = el [ headerBorder, headerPadding, Font.center ] <| text "Modality"
              , width = fill |> maximum 50
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.examType
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Name"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.patientName
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "NHI"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.nhi
              }
            , { header = el [ headerBorder, headerPadding, Font.alignLeft ] <| text "Examination"
              , width = fill |> maximum 1000 |> minimum 50
              , view =
                    \study ->
                        el [ rowPadding, Font.alignLeft ] <| text study.description
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Referral Time"
              , width = fill
              , view =
                    \study ->
                        case study.apptTime of
                            Just referralTime ->
                                el [ rowPadding, Font.center ] <| text (dateFormatter nz_zone referralTime)

                            Nothing ->
                                el [ rowPadding, Font.center ] <| text ""
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Triage Category"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.color <| rgb255 223 116 12, Font.center ] <|
                            text <|
                                triageCategoryToString study.triage
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Location"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.patientLoc
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
                                el [ rowPadding, Font.center, Background.color (rgba255 255 0 0 alpha) ] <|
                                    text (DateFormat.Relative.relativeTime time due)

                            Nothing ->
                                el [ rowPadding, Font.center ] <| text "-"
              }
            ]
        }


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
        , expect = Http.expectString GotData
        }


xmlDecoder : Decoder (List Study)
xmlDecoder =
    path [ "ereferral", "study" ] (list studyDecoder)


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
    Decode.map parseTriageCategory string


timeDecoder : Decoder Time.Posix
timeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                let
                    iso =
                        String.replace " " "T" str ++ "Z"
                in
                case Iso8601.toTime iso of
                    Err _ ->
                        Decode.fail "Time decoding failed"

                    Ok time ->
                        Decode.succeed time
            )


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
