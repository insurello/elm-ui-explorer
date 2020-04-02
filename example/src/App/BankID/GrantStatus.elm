module App.BankID.GrantStatus exposing (GrantStatus(..), Session)


type GrantStatus
    = Pending Session
    | Complete


type alias Session =
    { description : Maybe String }
