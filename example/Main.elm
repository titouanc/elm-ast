port module Main exposing (..)

import Html exposing (..)
import Html.App as App

import Ast
import Ast2Json
import Json.Encode
import Json.Decode

type Msg = ParseElm String

port replyJsonAst : String -> Cmd msg
port parseElmCode : (String -> msg) -> Sub msg

jsonify : String -> String
jsonify m =
    case Ast.parse m of
        ( Ok statements, _ ) -> 
            Json.Encode.encode 2 (Ast2Json.jmap Ast2Json.statement statements)

        err -> ""

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
    case msg of
        ParseElm s -> (Model, replyJsonAst (jsonify s))


type alias Model = {}
init : (Model, Cmd Msg)
init = (Model, Cmd.none)

view : Model -> Html Msg
view _ = div [] []

subscriptions : Model -> Sub Msg
subscriptions _ = parseElmCode ParseElm

main : Program Never
main = App.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions}
