module Main exposing (..)

import Html exposing (Html, text, div, h1, p, img, input, button)
import Html.Attributes exposing (src, type_, placeholder, defaultValue)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (Decoder, int, string)
import Http exposing (header)


---- MODEL ----


type alias Model =
  {
    name: String,
    health: Maybe Int
  }


init : ( Model, Cmd Msg )
init =
  ( { name = "elm-lang/core", health = Nothing }, Cmd.none )


---- UPDATE ----


type Msg
  = NoOp
  | Name String
  | GetRepoProfile
  | NewRepo (Result Http.Error (Maybe Int))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Name name ->
      ({ model | name = name }, Cmd.none)
    GetRepoProfile ->
      (model, getRepoProfile model.name)
    NewRepo (Ok repoHealth) ->
      ({ model | health = repoHealth }, Cmd.none)
    NewRepo (Err _) ->
      (model, Cmd.none)
    NoOp ->
      (model, Cmd.none)

getRepoProfile : String -> Cmd Msg
getRepoProfile repoName =
  let url = "https://api.github.com/repos/" ++ repoName ++ "/community/profile"
      acceptHeader = header "Accept" "application/vnd.github.black-panther-preview+json"
      request =
        Http.request
        { method = "GET"
        , headers = [acceptHeader]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decodeRepoProfileJson
        , timeout = Nothing
        , withCredentials = False
        }
  in Http.send NewRepo request

decodeRepoProfileJson : Decoder (Maybe Int)
decodeRepoProfileJson =
  Json.Decode.maybe (Json.Decode.at ["health_percentage"] Json.Decode.int)


---- VIEW ----


view : Model -> Html Msg
view model =
  div []
  [ h1 [] [ text "Repo Profile" ]
  , input [ type_ "text", placeholder "Repo name", defaultValue model.name, onInput Name ] []
  , button [ onClick GetRepoProfile ] [ text "Get repo profile" ]
  , div [] [
    h1 [] [ text model.name ]
    , p [] [ healthMessage model.health ]]]

healthMessage : Maybe Int -> Html msg
healthMessage health =
  case health of
    Nothing ->
      text "Submit a repo to see it's github community health rating"
    Just healthPercentage ->
      text ("Repo health: " ++ toString healthPercentage ++ "%")


---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
  { view = view
  , init = init
  , update = update
  , subscriptions = always Sub.none
  }
