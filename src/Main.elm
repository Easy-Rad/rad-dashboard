module Main exposing (Msg(..), getXml, init, main, subscriptions, update, view, viewTable, xmlDecoder)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Attribute, Html)
import Http
import Xml.Decode exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    List Study


type alias Study =
    { site : String
    , examType : String
    , description : String
    , nhi : String
    , patientName : String
    , urgency : String
    , orderDate : String
    , apptTime : String
    , patientType : String
    , patientLoc : String
    , notes : String
    , radNotes : String
    , triageStatus : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], getXml )



-- UPDATE


type Msg
    = GotData (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok rawXmlString) ->
            case run xmlDecoder rawXmlString of
                Ok referrals ->
                    ( referrals, Cmd.none )

                Err error ->
                    ( Debug.log ("Error" ++ error) [], Cmd.none )

        _ ->
            ( [], Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout []
        (column
            [ width fill ]
            [ el [ width fill, padding 20, Font.size 30 ] <| text "Radiology Dashboard"
            , viewTable model
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


headerSpacing : Element.Attribute Msg
headerSpacing =
    Element.padding 10


viewTable : Model -> Element Msg
viewTable model =
    Element.table []
        { data = model
        , columns =
            [ { header = el [ headerBorder, headerSpacing ] <| text "Exam Type"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text study.examType
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Site"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10, alignLeft ] <| text study.site
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Description"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10, Font.alignLeft ] <| text study.description
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Name"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10, Font.alignLeft ] <| text "John Doe"
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "NHI"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text "NHI0000"
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Urgency"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text study.urgency
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Appt Time"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text study.apptTime
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Patient Type"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text study.patientType
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Patient Location"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text study.patientLoc
              }
            , { header = el [ headerBorder, headerSpacing ] <| text "Triage Status"
              , width = fill
              , view =
                    \study ->
                        el [ padding 10, spacing 10 ] <| text study.triageStatus
              }
            ]
        }



-- HTTP


liveUrl : String
liveUrl =
    "http://159.117.39.240/apps/dashboard/monitorworklistdisplay-ereferrals-gen-ct-summary.xml"


getXml : Cmd Msg
getXml =
    Http.get
        { url = "summary.xml"
        , expect = Http.expectString GotData
        }


xmlDecoder : Decoder (List Study)
xmlDecoder =
    path [ "ereferral", "study" ] (list studyDecoder)


studyDecoder : Decoder Study
studyDecoder =
    succeed Study
        |> requiredPath [ "site" ] (single string)
        |> requiredPath [ "examtype" ] (single string)
        |> requiredPath [ "description" ] (single string)
        |> requiredPath [ "NHI" ] (single string)
        |> requiredPath [ "patientname" ] (single string)
        |> requiredPath [ "urgency" ] (single string)
        |> requiredPath [ "orderdate" ] (single string)
        |> requiredPath [ "appttime" ] (single string)
        |> requiredPath [ "pattype" ] (single string)
        |> requiredPath [ "patloc" ] (single string)
        |> requiredPath [ "notes" ] (single string)
        |> requiredPath [ "radnotes" ] (single string)
        |> requiredPath [ "triagestatus" ] (single string)
