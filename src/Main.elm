module Main exposing (Msg(..), fetchData, init, main, update, view, viewTable)

import Browser exposing (Document)
import DateFormat
import DateFormat.Relative
import Html exposing (Html, a, button, div, h1, img, input, label, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, for, id, scope, src, type_)
import Html.Events exposing (onCheck, onClick)
import Http exposing (Error(..), expectJson)
import Json.Decode exposing (Decoder, int, list, map, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)
import Task
import Time
import TimeZone


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Ok [])
        CT
        False
        Set.empty
        Set.empty
        Nothing
    , Task.perform FetchData Time.now
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
    , showHiddenReferrals : Bool
    , showRadNotes : Set Int
    , hiddenReferrals : Set Int
    , dataTimestamp : Maybe Time.Posix
    }


type alias Study =
    { id : Int
    , site : String
    , description : String
    , nhi : String
    , pa_firstname : String
    , pa_surname : String
    , pa_type : String
    , urgency : TriageCategory
    , received : Time.Posix
    , location : Maybe String
    , generalNotes : List String
    , radNotes : List String
    }


type Modality
    = CT
    | DSA
    | MR
    | NM
    | US
    | XR


type alias TriageCategory =
    Int


locationToTriageCategory : Maybe String -> TriageCategory
locationToTriageCategory location =
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
    | ShowReferral Int Bool
    | ShowRadNotes Int Bool
    | ShowHiddenReferrals Bool


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

        ShowReferral id show ->
            ( { model
                | hiddenReferrals =
                    (if show then
                        Set.remove

                     else
                        Set.insert
                    )
                        id
                        model.hiddenReferrals
              }
            , Cmd.none
            )

        ShowRadNotes id show ->
            ( { model
                | showRadNotes =
                    (if show then
                        Set.insert

                     else
                        Set.remove
                    )
                        id
                        model.showRadNotes
              }
            , Cmd.none
            )

        ShowHiddenReferrals show ->
            ( { model | showHiddenReferrals = show }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (60 * 1000) FetchData



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
    locationToTriageCategory study.location


sortByWaitingTime : Study -> Int
sortByWaitingTime study =
    Time.posixToMillis study.received


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


view : Model -> Document Msg
view model =
    let
        hiddenCount =
            case model.result of
                Ok studies ->
                    List.foldl
                        (\study acc ->
                            if Set.member study.id model.hiddenReferrals then
                                acc + 1

                            else
                                acc
                        )
                        0
                        studies

                _ ->
                    0
    in
    Document "Radiology Dashboard"
        [ div [ class "container-fluid" ]
            [ div [ class "d-flex flex-row align-items-center gap-4" ]
                ([ h1 [] [ text "Radiology Dashboard" ]
                 , viewModalityButtons model
                 ]
                    ++ (if hiddenCount == 0 then
                            []

                        else
                            [ div [ class "form-check", class "form-switch" ]
                                [ input [ type_ "checkbox", id "showHidden", class "form-check-input", checked model.showHiddenReferrals, onCheck ShowHiddenReferrals ] []
                                , label [ for "showHidden", class "form-check-label" ] [ "Show " ++ String.fromInt hiddenCount ++ " hidden" |> text ]
                                ]
                            ]
                       )
                    ++ (Maybe.map (viewTimeInfo >> div [ class "ms-auto" ] >> List.singleton) model.dataTimestamp |> Maybe.withDefault [])
                )
            , div [ class "row" ]
                [ case ( model.dataTimestamp, model.result ) of
                    ( Just dataTimestamp, Ok studies ) ->
                        viewTable dataTimestamp model studies

                    ( _, Err err ) ->
                        viewError err

                    ( Nothing, _ ) ->
                        text "Loading.."
                ]
            ]
        ]


viewModalityButton : Modality -> Modality -> Html Msg
viewModalityButton current modality =
    button
        [ onClick (ChangeModality modality)
        , class "btn"
        , class "btn-outline-primary"
        , classList
            [ ( "active", current == modality )
            ]
        ]
        [ text
            (case modality of
                XR ->
                    "XR"

                CT ->
                    "CT"

                MR ->
                    "MR"

                DSA ->
                    "DSA"

                NM ->
                    "NM"

                US ->
                    "US"
            )
        ]


viewModalityButtons : Model -> Html Msg
viewModalityButtons model =
    [ XR
    , CT
    , MR
    , US
    , NM
    , DSA
    ]
        |> List.map (viewModalityButton model.modality)
        |> div [ class "btn-group" ]


viewTimeInfo : Time.Posix -> List (Html Msg)
viewTimeInfo time =
    [ text "Last refresh: "
    , dateFormatter nz_zone time |> text
    ]


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


viewError : Http.Error -> Html Msg
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
        |> List.singleton
        |> p []


viewDue : Time.Posix -> Time.Posix -> String
viewDue t due =
    DateFormat.Relative.relativeTime t due


viewNotesCell : Bool -> Study -> List (Html Msg)
viewNotesCell showRadNotes study =
    let
        viewNotes : List String -> List String -> List (Html Msg)
        viewNotes notes classes =
            if List.isEmpty notes then
                []

            else
                notes |> List.intersperse " • " |> String.concat |> text |> List.singleton |> div (List.map class classes) |> List.singleton
    in
    viewNotes study.generalNotes []
        ++ (if List.isEmpty study.radNotes || not showRadNotes then
                []

            else
                viewNotes study.radNotes [ "text-primary" ]
           )


viewRow : Time.Posix -> Model -> Study -> Maybe (Html Msg)
viewRow time model study =
    let
        showRadNotes =
            Set.member study.id model.showRadNotes

        hidden =
            Set.member study.id model.hiddenReferrals

        due =
            dueInTime study
    in
    if model.showHiddenReferrals || not hidden then
        tr [ classList [ ( "table-secondary", hidden ) ] ]
            [ td []
                [ img
                    [ src
                        ("/visibility_"
                            ++ (if hidden then
                                    "off"

                                else
                                    "on"
                               )
                            ++ ".svg"
                        )
                    , class "btn"
                    , class "btn-light"
                    , class "btn-sm"
                    , onClick (ShowReferral study.id hidden)
                    ]
                    []
                ]
            , td [ class "user-select-all" ] [ text study.nhi ]
            , td [] [ study.pa_surname ++ ", " ++ study.pa_firstname |> text ]
            , td [] [ text study.description ]
            , td [] [ study.site ++ "-" ++ study.pa_type |> text ]
            , td [ classList [ ( "table-warning", locationToTriageCategory study.location <= 1 ) ] ] (study.location |> Maybe.map (text >> List.singleton) |> Maybe.withDefault [])
            , td [] [ dateFormatter nz_zone study.received |> text ]
            , td [ "table-" ++ triageCategoryToColourVariant study.urgency |> class ] [ triageCategoryToString study.urgency |> text ]
            , td [ classList [ ( "table-warning", due |> Maybe.map (Time.posixToMillis >> (>) (Time.posixToMillis time)) |> Maybe.withDefault False ) ] ] [ Maybe.map (viewDue time) due |> Maybe.withDefault "-" |> text ]
            , td [ class "w-25" ]
                [ div [ class "d-flex gap-2 align-items-start" ]
                    [ div []
                        [ img
                            [ src "/clinical_notes.svg"
                            , class "btn"
                            , class "btn-light"
                            , class "btn-sm"
                            , classList
                                [ ( "active", showRadNotes )
                                , ( "d-none", List.isEmpty study.radNotes )
                                ]
                            , onClick (not showRadNotes |> ShowRadNotes study.id)
                            ]
                            []
                        ]
                    , div [] <| viewNotesCell showRadNotes study
                    ]
                ]
            ]
            |> Just

    else
        Nothing


viewTable : Time.Posix -> Model -> List Study -> Html Msg
viewTable time model studies =
    [ [ ""
      , "NHI"
      , "Name"
      , "Examination"
      , "Site"
      , "Location"
      , "Referral Time"
      , "Triage Category"
      , "Due"
      , "Notes"
      ]
        |> List.map text
        |> List.map (List.singleton >> th [ scope "col" ])
        |> tr []
        |> List.singleton
        |> thead []
    , studies
        |> List.filterMap (viewRow time model)
        |> tbody []
    ]
        |> table [ class "table table-sm table-hover" ]


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
            referralTimeInPosix + locationToTriageCategory study.location * 60 * 60 * 1000
    in
    if triageHour >= 1009 then
        -- Ignore not triaged, planned etc.
        Nothing

    else
        Just (min triageBasedDue locationBasedDue |> Time.millisToPosix)



-- HTTP


fetchData : Time.Posix -> Modality -> Cmd Msg
fetchData timestamp modality =
    Http.get
        { url =
            "https://api.easyrad.duckdns.org/dashboard/"
                ++ (case modality of
                        XR ->
                            "XR"

                        CT ->
                            "CT"

                        MR ->
                            "MR"

                        DSA ->
                            "DS"

                        NM ->
                            "NM"

                        US ->
                            "US"
                   )
        , expect = expectJson (GotResult timestamp) (list studyDecoder |> Json.Decode.map sortByScore)
        }


triageCategoryToColourVariant : TriageCategory -> String
triageCategoryToColourVariant category =
    case category of
        0 ->
            "danger"

        1 ->
            "warning"

        4 ->
            "success"

        24 ->
            "info"

        _ ->
            "light"


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

        672 ->
            "4 weeks"

        1008 ->
            "6 weeks"

        1009 ->
            "DAROT"

        1010 ->
            "Planned"

        9999 ->
            "Not triaged"

        _ ->
            "-"


studyDecoder : Decoder Study
studyDecoder =
    succeed Study
        |> required "id" int
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
                "STAT" ->
                    0

                "1 hour" ->
                    1

                "4 hours" ->
                    4

                "24 hours" ->
                    24

                "2 days" ->
                    48

                "2 weeks" ->
                    336

                "4 weeks" ->
                    672

                "6 weeks" ->
                    1008

                "DAROT" ->
                    1009

                "Planned" ->
                    1010

                _ ->
                    99999
    in
    map parseTriageCategory string


posixDecoder : Decoder Time.Posix
posixDecoder =
    map (\i -> Time.millisToPosix (i * 1000)) int
