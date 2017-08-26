import Time
import Html exposing (..)
import Html.Attributes exposing (width, height, src, style, class, value, placeholder, id, href, attribute)
import Html.Events exposing (onClick, onInput)
import Array as A exposing (Array)
import Maybe as M

-- MODEL

type State = Initial | Thinking | Results
type alias Model =
    { state: State
    , thought: Int
    }

init : (Model, Cmd Msg)
init = (
    { state = Initial
    , thought = 0 }
    , Cmd.none)

thoughts : Array String
thoughts = A.fromList
    [ "Synthesizing a prioris..."
    , "Consulting microtubules..."
    , "Connecting to oracle..."
    , "Interacting with proof..."
    ]

numThoughts: Int
numThoughts = A.length thoughts

-- UPDATE

type Msg = Check | Reset | Tick Time.Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Check ->
        ({ model | state = Thinking }, Cmd.none)
    Reset ->
        ({ model | state = Initial }, Cmd.none)
    Tick t ->
        if model.state == Thinking && (secondsPassed t 1)
        then if model.thought == numThoughts - 1
            then ({model | state = Results}, Cmd.none)
            else ({ model | thought = model.thought + 1}, Cmd.none)
        else (model, Cmd.none)

secondsPassed : Time.Time -> Int -> Bool
secondsPassed t n = (rem (round <| Time.inSeconds t) n) == 0

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ div [ id "container" ] <|
            [ h1 [] [text "Has P = NP been solved yet?"]
            ] ++ (gui model)
        , p [ id "footer" ]
            [ text "Made by "
            , a [href "twitter.com/adam_chal"] [text "@adam_chal"]
            , text ", with apologies to "
            , a [href "https://www.scottaaronson.com/blog"] [text "Scott Aaronson"]
            , text ". Source available "
            , a [href ""] [text "here"]
            , text "."
            ]
        ]

gui : Model -> List (Html Msg)
gui model =
    case model.state of
    Initial ->
        [ div [id "buttons"]
            [ input [placeholder "Enter the URL of the paper you'd like to check"] []
            , button [onClick Check] [text "Check"]
            ]
        ]
    Thinking ->
        let
            thought = M.withDefault "Synthesizing a prioris..." (A.get model.thought thoughts)
        in
            [ img [src "assets/spinner.gif", width 15, height 15] []
            , span [id "thoughts"] [text thought]
            ]

    Results ->
        [ h2 [] [text "NO"]
        , p []
            [ text "This paper does not, in fact, prove P = NP. A full report has been generated "
            , a [href "https://www.scottaaronson.com/blog/?p=304"] [text "here"]
            , text "."
            ]
        ]

stylesheet = node "link" [ attribute "rel" "stylesheet", attribute "property" "stylesheet", attribute "href" "style.css"] []


-- WIRING

subs : Model -> Sub Msg
subs model = Time.every Time.second Tick

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subs
    }