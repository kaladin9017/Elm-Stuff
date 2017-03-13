import Html exposing (..)


-- Model - the state of my application

type alias Model = { ... }

-- Update - a way of updating state of application

type Msg = Reset | ...

update : Msg -> Model -> Model
update : msg model =
  case msg of
    Reset -> ...
    ...

-- View - a way to view your state as HTML

view : Model -> Html Msg
view model =
  ...
    
