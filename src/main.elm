import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random exposing (Seed, int, step)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Time
import Array
import Process exposing (sleep)
import List.Extra exposing (getAt, removeAt)
import Random.List
import Debug

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { wordCount : Int,
    displayTime : Float,
    startFlag : Int,
    readyTime : Int,
    database : List Card,
    problemSubstitution : List Card,
    problems : List Card,
    randomNumber : Int,
    rightCount : Int,
    selectedCount : Int,
    databaseCount : Int
  }

type alias Card =
  { id : Int
  , value : String
  , flag : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 3 0.3 0 4 
          [
            { id = 1, value = "abattage", flag = -2 },
            { id = 2, value = "abbacomes", flag = -2 },
            { id = 3, value = "abbotnullius", flag = -2 },
            { id = 4, value = "Abdul-baha", flag = -2 },
            { id = 5, value = "abecedaries", flag = -2 },
            { id = 6, value = "Maseru", flag = -2 },
            { id = 7, value = "masjid", flag = -2 },
            { id = 8, value = "Masontown", flag = -2 },
            { id = 9, value = "ptyalocele", flag = -2 },
            { id = 10, value = "ptisan", flag = -2 },
            { id = 11, value = "infecter", flag = -2 },
            { id = 12, value = "infects", flag = -2 },
            { id = 13, value = "infelicities", flag = -2 },
            { id = 14, value = "infector", flag = -2 },
            { id = 15, value = "infertilely", flag = -2 },
            { id = 16, value = "infiltrating", flag = -2 },
            { id = 17, value = "suppositions", flag = -2 },
            { id = 18, value = "suppositive", flag = -2 },
            { id = 19, value = "suppressen", flag = -2 },
            { id = 20, value = "Wisconsin", flag = -2 },
            { id = 21, value = "disincrustant", flag = -2 },
            { id = 22, value = "imbitters", flag = -2 },
            { id = 23, value = "releasers", flag = -2 },
            { id = 24, value = "soldo", flag = -2 },
            { id = 25, value = "solecism", flag = -2 },
            { id = 26, value = "solei", flag = -2 },
            { id = 27, value = "solenesses", flag = -2 },
            { id = 28, value = "solenoglyphic", flag = -2 },
            { id = 29, value = "soliciting", flag = -2 },
            { id = 30, value = "Zworykin", flag = -2 }]
         [] [] 7 0 0 30
    , Cmd.none 
  )
 
get : Int -> List a -> Maybe a
get nth list =
    list
        |> List.drop (nth - 1)
        |> List.head
-- UPDATE

type Msg = ChangeWordCount String | ChangeDisplayTime String | StartGame | Tick Time.Posix | GenerateRandom | RandomNumber Int | ShowSubstitution | SelectAnswer Card | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
  NoOp ->
      ( model, Cmd.none )
  ChangeWordCount wordCount ->
      ({ model | wordCount = fromJust (String.toInt wordCount) }, Cmd.none)
  ChangeDisplayTime displayTime ->
      ({ model | displayTime =  fromJust (String.toFloat displayTime) }, Cmd.none)
  StartGame ->
      let
          initData e =
              if (e.id >= (modBy (model.databaseCount - 1) model.randomNumber) + 1 && e.id < (modBy (model.databaseCount - 1) model.randomNumber) + 20 + 1) then
                   if (e.id >= (modBy 20 model.randomNumber * 2) + 1 && e.id < ((modBy 20 model.randomNumber * 2) + model.wordCount) + 1) then
                      { e | flag = -1 }
                    else
                      { e | flag = 0 }
              else 
                { e | flag = -2 }
             
          initDatabase =
              List.map initData model.database

          -- initCard e =
          --     if (e.id >= (modBy 20 model.randomNumber) + 1 && e.id < (modBy 20 model.randomNumber + model.wordCount) + 1) then
          --       { e | flag = -1 }
          --     else 
          --       { e | flag = 0 }
             
          -- initSubstitution =
          --     List.map initCard model.problemSubstitution
          initUnit e =
                { e | flag = -2 }
             
          initialize =
              List.map initUnit model.database
      in
        ({ 
          model | database = if (model.startFlag > 0) then initialize else initDatabase,
          problemSubstitution = List.reverse initDatabase,
          startFlag = if (model.wordCount > 0 && model.displayTime > 0) then 1 else 0,
          rightCount = 0,
          selectedCount = 0,
          readyTime = 4,
          problems = initDatabase
        }, Random.generate RandomNumber (Random.int 0 19))
  Tick newTime ->
      ({  model | readyTime = if (model.readyTime > 0 && model.startFlag == 1) then model.readyTime - 1 else if (model.readyTime == 0) then 0 else 3 }, Cmd.none)
  GenerateRandom ->
      ( model
      , Random.generate RandomNumber (Random.int 0 19)
      )
  RandomNumber rn ->
      ( { model | randomNumber = rn }
      , Cmd.none
      )
  ShowSubstitution ->
      ({
        model | startFlag = 2
      }, Cmd.none)
  SelectAnswer card ->
      let
          selectCard e =
              if e.id == card.id && e.flag == 0 then
                  { e | flag = 1 }
              else if e.id == card.id && e.flag == -1 then
                  { e | flag = 2 } 
              else
                  e
             
          updateSubstitution =
              List.map selectCard model.problemSubstitution

          numberComparing =
                    let
                        i =
                            0
                    in
                        List.length <|
                            List.filter (\val -> val == 1) <|
                                List.map
                                    (\x ->
                                        if x.flag == 2 then
                                            i + 1
                                        else
                                            i
                                    )
                                    updateSubstitution
      in
        ({
          model
              | problemSubstitution =
                            List.map
                                (\x -> 
                                if x.id == card.id && x.flag == 0 then
                                    { x | flag = 1 }
                                else if x.id == card.id && x.flag == -1 then
                                    { x | flag = 2 } 
                                else
                                    x
                                )
                                model.problemSubstitution,

          selectedCount = model.selectedCount + 1, 
          rightCount = numberComparing

        }, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.selectedCount == model.wordCount && model.readyTime == 0  then Time.every 3000 (always StartGame)
  else if model.readyTime == 0 then Time.every (model.displayTime * 1000) (always ShowSubstitution)
  else Time.every 1000 Tick

-- VIEW

view : Model -> Html Msg
view model =
  viewValidation model ChangeWordCount ChangeDisplayTime StartGame SelectAnswer NoOp

viewValidation : Model -> (String -> msg)  -> (String -> msg) -> msg -> (Card -> msg) -> msg -> Html msg
viewValidation model changeWordCount changeDisplayTime startGame selectAnswer noOp =
  if model.startFlag == 0 then
    div [ style "text-align" "center", style "margin-top" "200px"]
    [ label [style "width" "200px", style "font-size" "20px"][ text " Count of Words "] 
    , input [ placeholder "Enter", value (String.fromInt model.wordCount), style "width" "150px", style "height" "30px", style "font-size" "18px", style "text-align" "center", onInput changeWordCount ] []
    , div [style "margin-top" "10px"] [ label [style "width" "200px", style "text-align" "right", style "font-size" "20px"][ text " Display Time "] 
    , input [ placeholder "Enter", value (String.fromFloat model.displayTime), style "width" "150px", style "height" "30px", style "font-size" "18px", style "text-align" "center", style "margin-left" "17px", onInput changeDisplayTime ] [] ]
    , div [style "margin-top" "40px"]
    [
        button [ style "width" "100px", style "height" "30px", style "font-size" "16px", style "backgroundColor" "#ddd", onClick startGame] [ text "Start" ]
    ]
    ]
  else if model.readyTime > 0 then
    div [ style "margin" "20px", style "font-size" "60px", style "text-align" "center"] 
    [ text (String.fromInt(model.readyTime)) ]
  else if model.startFlag == 1 then
      let 
        getProblemItem problem =
            if (problem.flag == -1) then
              div []
                  [span [style "width" "200px", style "font-size" "30px"][ text <| problem.value ] ]
            else
              div [][]
      in
        div [ style "margin" "50px", style "font-size" "40px"] 
        [ 
          table [ style "width" "100%" ]
          [tr [ style "width" "100%" ]
          [td [ style "width" "50%" ]
          [
              div []
              <|
                        List.map
                            getProblemItem
                        <|
                            model.problems
          ],
          td [ style "width" "50%" ]
          []
          ]]
        ]  
  else if model.startFlag == 2 && model.selectedCount < model.wordCount then
    let 
      getSubstitutionItem substitution = 
        if (substitution.flag > -2) then
          div [ style "width" "160px", style "height" "40px", style "float" "left" ]
            [button 
              [ style "width" "150px", style "height" "30px", style "font-size" "16px", style "backgroundColor" "#ddd", 
                if (substitution.flag == 1) then style "color" "#ff0000" else if (substitution.flag == 2) then style "color" "#00ff00" else style "color" "#000000",
                    onClick <|
                      (if substitution.flag <= 0 then
                          selectAnswer
                      else
                          always noOp
                      )
                      <|
                          substitution
              ] [ text <| substitution.value ]]
        else
          div[][]
    in
        div [ style "margin" "50px", style "font-size" "40px"] 
          [ 
            table [ style "width" "100%" ]
            [tr [ style "width" "100%" ]
            [td [ style "width" "50%" ]
            [],
            td [ style "width" "50%" ]
            [
                div []
                <|
                      List.map
                          getSubstitutionItem
                      <|
                          model.problemSubstitution
            ]
            ]]
          ]
  else 
    let
        getProblemItem problem =
          if problem.flag == -1 then
            div []
                [span [style "width" "200px", style "font-size" "30px"][ text <| problem.value ]]
          else
            div [][]

        getSubstitutionItem substitution = 
           if (substitution.flag > -2) then
              div 
              [ style "width" "160px", style "height" "40px", style "float" "left" ]
              [button [ style "width" "150px", style "height" "30px", style "font-size" "16px", style "backgroundColor" "#ddd",
              if (substitution.flag == 1) then style "color" "#ff0000" else if (substitution.flag == 2) then style "color" "#00ff00" else style "color" "#000000"
              ] [ text <| substitution.value ]]
            else
              div [] []
    in
        div [ style "margin" "50px", style "font-size" "40px"] 
            [ 
              div [ style "text-align" "center "]
              [
                span [style "width" "200px", style "font-size" "30px"][ text ("acuracy = " ++ String.fromInt model.rightCount  ++ " / " ++ String.fromInt model.wordCount) ]
              ],

              table [ style "width" "100%" ]
              [tr [ style "width" "100%" ]
              [td [ style "width" "50%" ]
              [
                div []
                <|
                          List.map
                              getProblemItem
                          <|
                              model.problems
              ],
              td [ style "width" "50%" ]
              [
                  div []
                    <|
                              List.map
                                  getSubstitutionItem
                              <|
                                  model.problemSubstitution
              ]
              ]]
            ]

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.todo "error: fromJust Nothing"
    