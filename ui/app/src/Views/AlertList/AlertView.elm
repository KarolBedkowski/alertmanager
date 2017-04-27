module Views.AlertList.AlertView exposing (view)

import Alerts.Types exposing (Alert)
import Html exposing (..)
import Html.Attributes exposing (class, style, href)
import Html.Events exposing (onClick)
import Types exposing (Msg(CreateSilenceFromAlert, Noop, MsgForAlertList))
import Utils.Date
import Views.AlertList.Types exposing (AlertListMsg(AddFilterMatcher))
import Utils.Views exposing (buttonLink, onClickMsgButton, listButton)
import Utils.Filter
import Time exposing (Time)
import Date.Extra.Format
import Date.Extra.Config.Config_en_us exposing (config)
import Date


view : Alert -> Html Msg
view alert =
    li
        [ class "align-items-center list-group-item alert-list-item p-0 d-inline-flex justify-content-start"
        ]
        [ dateView alert.startsAt
        , labelButtons alert.labels
        , div [ class "ml-auto d-inline-flex align-self-stretch p-2" ]
            [ generatorUrlButton alert.generatorUrl
            , silenceButton alert
            ]
        ]


dateView : Time -> Html Msg
dateView time =
    i
        [ class "h-100  d-flex flex-column justify-content-center p-2 text-muted"
        , style [ ( "border-right", "1px solid #ccc" ), ( "font-family", "monospace" ) ]
        ]
        [ span [] [ text <| Date.Extra.Format.format config Date.Extra.Format.isoTimeFormat (Date.fromTime time) ]
        , small [] [ text <| Date.Extra.Format.format config Date.Extra.Format.isoDateFormat (Date.fromTime time)]
        ]


labelButtons : List ( String, String ) -> Html Msg
labelButtons labels =
    let
        -- the alert name label should be first
        sortedLabels =
            List.append
                (List.filter (Tuple.first >> (==) "alertname") labels)
                (List.filter (Tuple.first >> (/=) "alertname") labels)
    in
        div [] <| List.map labelButton sortedLabels


labelButton : ( String, String ) -> Html Msg
labelButton ( key, value ) =
    let
        msg =
            AddFilterMatcher False
                { key = key
                , op = Utils.Filter.Eq
                , value = value
                }
                |> MsgForAlertList

        label =
            if key == "alertname" then
                span [ class "badge badge-primary" ]
                    [ i [ class "fa fa-tag pr-1" ] [], text value ]
            else
                span [ class " badge badge-warning" ]
                    [ i [ class "fa fa-tag pr-1" ] [], text (key ++ "=" ++ value) ]
    in
        span [ class "pl-2" ]
            [ label ]


silenceButton : Alert -> Html Msg
silenceButton alert =
    let
        id =
            Maybe.withDefault "" alert.silenceId
    in
        if alert.silenced then
            buttonLink "fa-deaf" ("#/silences/" ++ id) "blue" Noop
        else
            button [ class "h-100 btn btn-warning rounded-0", style [], href "#/silences/new?keep=1", onClick (CreateSilenceFromAlert alert) ] [ span [ class "fa fa-bell-slash-o" ] [] ]


generatorUrlButton : String -> Html Msg
generatorUrlButton url =
    a
        [ class "h-100 btn btn-primary rounded-0 align-items-center d-inline-flex border-right-0"
        , href url
        ]
        [ i [ class "fa fa-line-chart" ] [] ]
