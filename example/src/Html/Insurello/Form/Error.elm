module Html.Insurello.Form.Error exposing (ValidationError(..), ValidationErrorPersonName(..), errorString)

import Form.Error exposing (ErrorValue(..))
import Shared.SwedishPersonalNumber as PersonalNumber


type ValidationError
    = InvalidPersonalNumber PersonalNumber.ValidationError
    | InvalidISODate (Maybe String)
    | InvalidISOTime String
    | EmptyChoice
    | EmptyList
    | EmptySet
    | InvalidSet
    | AcceptTerms
    | InvalidAge Int
    | InvalidPersonName ValidationErrorPersonName
    | InvalidPhone
    | InvalidRelation
    | CustomErrorMessage String


type ValidationErrorPersonName
    = LastNameMissing
    | NameTooShort


errorString : ErrorValue ValidationError -> String
errorString error =
    defaultErrorString error customErrorString


defaultErrorString : ErrorValue e -> (e -> String) -> String
defaultErrorString error stringFromCustomError =
    case error of
        Empty ->
            "Fältet får inte vara tomt"

        InvalidString ->
            "Fältet får inte vara tomt"

        InvalidEmail ->
            "Det är inte en giltig e-postadress"

        InvalidFormat ->
            "Fältet har inte rätt format"

        InvalidInt ->
            "Kan inte innehålla några andra tecken än siffror"

        InvalidFloat ->
            "Får bara innehålla siffror, och ett kommatecken om du vill"

        InvalidBool ->
            "Du måste svara ja eller nej"

        SmallerIntThan n ->
            "För lågt, måste vara " ++ String.fromInt n ++ " eller högre"

        GreaterIntThan n ->
            "För högt, måste vara " ++ String.fromInt n ++ " eller lägre"

        SmallerFloatThan n ->
            "För lågt, måste vara " ++ String.fromFloat n ++ " eller högre"

        GreaterFloatThan n ->
            "För högt, måste vara " ++ String.fromFloat n ++ " eller lägre"

        ShorterStringThan n ->
            "Måste vara minst " ++ String.fromInt n ++ " tecken"

        LongerStringThan n ->
            "Får inte vara längre än " ++ String.fromInt n ++ " tecken"

        NotIncludedIn ->
            "Är inte ett korrekt val från listan"

        CustomError e ->
            stringFromCustomError e


customErrorString customError =
    case customError of
        InvalidISODate maybeMessage ->
            maybeMessage |> Maybe.withDefault "Det är inte ett giltigt datum"

        InvalidPersonalNumber PersonalNumber.InvalidFormat ->
            "Felaktigt format för ett svenskt personummer"

        InvalidPersonalNumber PersonalNumber.InvalidLength ->
            "Personnumret har fel längd"

        InvalidPersonalNumber (PersonalNumber.InvalidDate _) ->
            "Personnumret måste börja med ett datum"

        InvalidPersonalNumber PersonalNumber.InvalidChecksum ->
            "En eller flera siffor i personummret är fel"

        InvalidISOTime _ ->
            "Det är inte ett giltig klockslag. Behöver vara i formatet hh:mm - t.ex 15:30"

        EmptyChoice ->
            "Du måste göra ett val"

        EmptyList ->
            "Du måste lägga till minst en sak i listan"

        EmptySet ->
            "Du måste göra minst ett val"

        InvalidSet ->
            "Den kombination av val du har gjort är inte tillåten"

        AcceptTerms ->
            "Du måste acceptera användarvillkoren"

        InvalidAge age ->
            "Du måste vara " ++ String.fromInt age ++ " eller äldre"

        InvalidPersonName LastNameMissing ->
            "Du behöver ange både för- och efternamn"

        InvalidPersonName NameTooShort ->
            "Namnet behöver bestå av minst 3 tecken"

        InvalidPhone ->
            "Telefonnumret behöver bestå av minst 8 siffror"

        InvalidRelation ->
            "Du kan bara göra en anmälan åt dig själv, ditt barn eller en person du är förmyndare till"

        CustomErrorMessage string ->
            string
