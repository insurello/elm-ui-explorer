module Html.Insurello.Form.Validation exposing (minAge, personalNumber, set)

import Form.Validate exposing (..)
import Html.Insurello.Form.Error exposing (ValidationError(..), ValidationErrorPersonName(..))
import Set exposing (Set)
import Shared.SwedishPersonalNumber as PersonalNumber exposing (PersonalNumber)
import Time
import Time.Extra


personalNumber : Form.Validate.Validation ValidationError PersonalNumber
personalNumber =
    let
        validate str =
            PersonalNumber.fromString str
                |> Result.mapError InvalidPersonalNumber
                |> Result.mapError customError
    in
    customValidation string validate


minAge : Int -> Time.Posix -> PersonalNumber -> Form.Validate.Validation ValidationError PersonalNumber
minAge min now pnr _ =
    let
        error =
            Err (customError (InvalidAge min))
    in
    case PersonalNumber.toDate pnr of
        Err _ ->
            error

        Ok date ->
            let
                age =
                    Time.Extra.diff Time.Extra.Year Time.utc date now
            in
            if age >= min then
                Ok pnr

            else
                error


set :
    List String
    -> Form.Validate.Validation ValidationError (Set.Set String)
set values =
    let
        field value =
            Form.Validate.field value bool
                |> Form.Validate.map (\v -> ( v, value ))
    in
    List.map field values
        |> Form.Validate.sequence
        |> Form.Validate.map (\list -> List.filter Tuple.first list)
        |> Form.Validate.map (\list -> List.map Tuple.second list)
        |> Form.Validate.map Set.fromList
