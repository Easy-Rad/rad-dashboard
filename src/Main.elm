module Main exposing (Msg(..), getXml, init, main, subscriptions, update, view, viewTable, xmlDecoder)

import Browser
import Date exposing (Date)
import Element exposing (Element, alignLeft, alignRight, centerY, column, el, fill, padding, rgb255, row, scrollbarX, scrollbarY, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Html exposing (Attribute, Html)
import Http
import Xml.Decode as Decode exposing (Decoder, int, list, oneOf, path, requiredPath, run, single, string, succeed)



-- TODOs
-- 1. Notes and Radnotes parsing, because they are fully embedded html
-- MAIN


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "ALL", getXml )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { studies : List Study
    , modality : String
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
    , apptTime : String
    , patientType : String
    , patientLoc : String
    , triageStatus : String
    }


type alias TriageCategory =
    Int



-- UPDATE


type Msg
    = GotData (Result Http.Error String)
    | FilterModality String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok rawXmlString) ->
            case run xmlDecoder rawXmlString of
                Ok referrals ->
                    ( { model | studies = referrals }, Cmd.none )

                Err error ->
                    ( Debug.log ("Error" ++ error) Model [] "all", Cmd.none )

        FilterModality modality ->
            ( { model | modality = modality }, Cmd.none )

        _ ->
            ( Debug.log "Unmatched message type" model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        , scrollbarY
        ]
        (column
            [ width fill ]
            [ el
                [ width fill
                , padding 20
                , Font.size 30
                , Font.family [ Font.typeface "Orbitron", Font.sansSerif ]
                ]
              <|
                text "Radiology Dashboard"
            , row [ padding 30, spacing 20 ]
                [ Input.button [] { onPress = Just <| FilterModality "ALL", label = text "All" }
                , Input.button [] { onPress = Just <| FilterModality "XR", label = text "XR" }
                , Input.button [] { onPress = Just <| FilterModality "CT", label = text "CT" }
                , Input.button [] { onPress = Just <| FilterModality "MR", label = text "MR" }
                ]
            , el [ padding 10 ] <| lazy viewTable studies
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
    Element.paddingEach { top = 20, right = 5, left = 5, bottom = 5 }


viewTable : List Study -> Element Msg
viewTable studies =
    Element.table
        [ padding 20
        , Border.color <| rgb255 111 195 223
        , Border.width 2
        , Border.rounded 10
        ]
        { data = studies
        , columns =
            [ { header = el [ headerBorder, headerPadding, Font.center ] <| text "Modality"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.examType
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Site"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.site
              }
            , { header = el [ headerBorder, headerPadding, Font.alignLeft ] <| text "Description"
              , width = fill |> Element.maximum 100
              , view =
                    \study ->
                        el [ rowPadding, Font.alignLeft ] <| text study.description
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
            , { header = el [ headerBorder, headerPadding ] <| text "Triage Category"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.color <| rgb255 223 116 12 ] <|
                            text <|
                                triageCategoryToString study.triage
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Appt Time"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.apptTime
              }
            , { header = el [ headerBorder, headerPadding ] <| text "Pt Type"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.patientType
              }
            , { header = el [ headerBorder, headerPadding ] <| text "Pt Location"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text study.patientLoc
              }
            , { header = el [ headerBorder, headerPadding ] <| text "Triage Status"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding ] <|
                            el
                                [ Font.size 15
                                , Font.color <| rgb255 0 0 0
                                , Background.color <| rgb255 0 255 0
                                , padding 3
                                , Border.rounded 5
                                ]
                            <|
                                text study.triageStatus
              }
            ]
        }



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

        2 ->
            "4 hours"

        3 ->
            "24 hours"

        4 ->
            "2 days"

        5 ->
            "2 weeks"

        6 ->
            "Planned"

        9 ->
            "Not triaged"

        _ ->
            "Unknown"


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
                    2

                "24 HOURS" ->
                    3

                "2 DAYS" ->
                    4

                "2 WEEKS" ->
                    5

                "PLANNED" ->
                    6

                "NOTTRIAGED" ->
                    9

                _ ->
                    Debug.log ("Unknown triage category (status): " ++ triage) 99
    in
    Decode.map parseTriageCategory string


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
        |> requiredPath [ "appttime" ] (single string)
        |> requiredPath [ "pattype" ] (single string)
        |> requiredPath [ "patloc" ] (single string)
        |> requiredPath [ "triagestatus" ] (single string)
