import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL
type alias Model =
  { todo : String
  , todos : List String
  }

model : Model
model =
  { todo = "",
  todos= []
  }

-- UPDATE
stylesheet =
  let
   tag =
     "link"

   attrs =
     [ attribute "Rel" "stylesheet"
     , attribute "property" "stylesheet"
     , attribute "href" "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
     ]

   children =
     []
 in
  node tag attrs children


type Msg =
  UpdateTodo String
  | AddTodo
  | RemoveAll
  | RemoveItem String
  | Clear
update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTodo text ->
      { model | todo = text }
    AddTodo ->
      { model | todos = model.todo :: model.todos }
    RemoveAll ->
      { model | todos = [] }
    RemoveItem text ->
      { model | todos = List.filter (\x -> x /= text) model.todos }
    Clear ->
      { model | todo = "" }


-- VIEW
todoItem : String -> Html Msg
todoItem todo =
  li [] [ text todo, button [ onClick (RemoveItem todo) ] [ text "x" ] ]

todoList : List String -> Html Msg
todoList todos =
  let
   child =
     List.map todoItem todos
  in
   ul [] child

view model =
  div [ class "jumbotron" ]
   [ stylesheet
   , input [ type_ "text"
           , onInput  UpdateTodo
           , value model.todo
           , onMouseEnter Clear
           ] []
   , br [] []
   , button [ onClick AddTodo, class "btn btn-primary" ] [ text "Submit" ]
   , button [ onClick RemoveAll, class "btn btn-danger" ] [ text "Remove All" ]
   , div [] [ todoList model.todos ]
   ]
