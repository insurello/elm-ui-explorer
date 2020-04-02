module App.BankID exposing (Model(..), Msg(..), init, start, update)

import App.ApiRequest as ApiRequest
import App.BankID.Grant exposing (Grant)
import App.BankID.GrantStatus exposing (GrantStatus)
import App.UserAgent exposing (UserAgent)
import Process
import Shared.SwedishPersonalNumber exposing (PersonalNumber)
import Task
import Url exposing (Url)


type Model
    = Start (Maybe String)
    | Pending Grant (Maybe App.BankID.GrantStatus.Session)
    | Failed Grant ApiRequest.Error
    | Complete


type Msg
    = Cancel
    | Retry
    | Collect
    | Response (Result ApiRequest.Error GrantStatus)


init : Model
init =
    Start Nothing


start : UserAgent -> Url -> PersonalNumber -> ( Model, Cmd Msg )
start userAgent redirectUrl personalNumber =
    let
        grant =
            App.BankID.Grant.fromPersonalNumber personalNumber
    in
    ( Pending grant Nothing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Pending grant (Just session), Collect ) ->
            ( Pending grant (Just session)
            , Cmd.none
            )

        ( Pending grant Nothing, Response (Ok (App.BankID.GrantStatus.Pending session)) ) ->
            ( Pending grant (Just session)
            , Cmd.batch
                [ delay 1000 Collect
                ]
            )

        ( Pending grant (Just _), Response (Ok (App.BankID.GrantStatus.Pending session)) ) ->
            ( Pending grant (Just session), delay 1000 Collect )

        ( Pending _ _, Response (Ok App.BankID.GrantStatus.Complete) ) ->
            ( Complete, Cmd.none )

        ( Pending grant session, Response (Err ApiRequest.NetworkError) ) ->
            ( Pending grant session, delay 1000 Collect )

        ( Pending grant _, Response (Err error) ) ->
            ( Failed grant error
            , Cmd.none
            )

        ( Pending grant _, Cancel ) ->
            ( Start Nothing, Cmd.none )

        ( Failed grant _, Cancel ) ->
            ( Start Nothing, Cmd.none )

        ( Failed grant _, Retry ) ->
            ( Pending grant Nothing
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


delay : Float -> msg -> Cmd msg
delay duration msg =
    Task.perform (always msg) (Process.sleep duration)
