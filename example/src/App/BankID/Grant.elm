module App.BankID.Grant exposing
    ( Grant
    , fromPersonalNumber
    )

import Shared.SwedishPersonalNumber exposing (PersonalNumber)


type Grant
    = Grant
        { personalNumber : PersonalNumber
        , redirectUri : Maybe String
        }


fromPersonalNumber : PersonalNumber -> Grant
fromPersonalNumber personalNumber =
    Grant
        { personalNumber = personalNumber
        , redirectUri = Nothing
        }
