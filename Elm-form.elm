import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Regex

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordCheck : String
  }

model : Model
model =
  Model "" "" ""


-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordCheck String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordCheck passwordCheck ->
      { model | passwordCheck = passwordCheck }


-- VIEW

view: Model -> Html Msg
view model =
  div []
  [ input [ placeholder "Enter Name" , onInput Name ] []
  , input [ type_ "password", placeholder "Enter Password" , onInput Password ] []
  , input [ type_ "password", placeholder "Enter Password Again" , onInput PasswordCheck ] []
  , viewValidation model
  ]

viewValidation: Model -> Html msg
viewValidation model =
  let
   (color, message) =
     if model.password == model.passwordCheck && String.length model.password > 8 && String.contains "D" model.password then
       ("green", "OK")
     else
       ("red", "Incorrect Password")
  in
   div [ style [("color", color)] ] [ text message ]
