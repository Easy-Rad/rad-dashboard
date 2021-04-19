module Main exposing (Msg(..), getXml, init, main, subscriptions, update, view, viewTable, xmlDecoder)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
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
    Element.layout
        [ Background.color <| rgb255 12 20 31
        , Font.color <| rgb255 230 255 255
        , Font.family [ Font.typeface "Roboto", Font.sansSerif ]
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
            , el [ padding 10 ] <| viewTable model
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


viewTable : Model -> Element Msg
viewTable model =
    Element.table
        [ padding 20
        , Border.color <| rgb255 111 195 223
        , Border.width 2
        , Border.rounded 10
        ]
        { data = model
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
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.alignLeft ] <| text study.description
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "Name"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text "John Doe"
              }
            , { header = el [ headerBorder, headerPadding, Font.center ] <| text "NHI"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.center ] <| text "NHI0000"
              }
            , { header = el [ headerBorder, headerPadding ] <| text "Urgency"
              , width = fill
              , view =
                    \study ->
                        el [ rowPadding, Font.color <| rgb255 223 116 12 ] <| text study.urgency
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
