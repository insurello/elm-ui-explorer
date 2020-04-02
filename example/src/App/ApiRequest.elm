module App.ApiRequest exposing (BadStatusPayload(..), Error(..))


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int BadStatusPayload
    | BadBody String


type BadStatusPayload
    = StringPayload String
    | JsonPayload BadStatusResponse


type alias BadStatusResponse =
    { error : String
    , errorDescription : String
    }
