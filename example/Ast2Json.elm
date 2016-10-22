module Ast2Json exposing (..)

import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Json.Encode as Json

jmap : (a -> Json.Value) -> List a -> Json.Value
jmap f l = Json.list (List.map f l)

--type Type
--  = TypeConstructor QualifiedType (List Type)
--  | TypeVariable Name
--  | TypeRecordConstructor Type (List (Name, Type))
--  | TypeRecord (List (Name, Type))
--  | TypeTuple (List Type)
--  | TypeApplication Type Type

elmtype : Type -> Json.Value
elmtype t =
    case t of
        TypeApplication f t ->
            Json.object [("ast_type", Json.string "TypeApplication"),
                         ("from", elmtype t),
                         ("to", elmtype t)]

        TypeVariable n ->
            Json.object [("ast_type", Json.string "TypeVariable"),
                         ("name", Json.string n)]

        TypeConstructor n types ->
            Json.object [("ast_type", Json.string "TypeConstructor"),
                         ("name", jmap Json.string n),
                         ("subtypes", jmap elmtype types)]

        t -> Json.string (toString e)

--type Expression
--  = Character Char
--  | String String
--  | Integer Int
--  | Float Float
--  | Variable (List Name)
--  | Range Expression Expression
--  | List (List Expression)
--  | Access Expression (List Name)
--  | Record (List (Name, Expression))
--  | RecordUpdate Name (List (Name, Expression))
--  | If Expression Expression Expression
--  | Let (List (Name, Expression)) Expression
--  | Case Expression (List (Expression, Expression))
--  | Lambda (List Name) Expression
--  | Application Expression Expression
--  | BinOp Expression Expression Expression

expression : Expression -> Json.Value
expression e =
    case e of
        String s  -> Json.string s
        Integer i -> Json.int i
        Float f   -> Json.float f
        Variable n ->
            Json.object [("ast_type", Json.string "Variable"),
                         ("identifier", jmap Json.string n)]

        BinOp o l r ->
            Json.object [("ast_type", Json.string "Infix"),
                         ("left", expression l),
                         ("op", expression o),
                         ("right", expression r)]

        Range e1 e2 ->
            Json.object [("ast_type", Json.string "Range"),
                         ("from", expression e1),
                         ("to", expression e2)]

        List es ->
            Json.object [("ast_type", Json.string "List"),
                         ("content", Json.list (List.map expression es))]

        Application e1 e2 ->
            Json.object [("ast_type", Json.string "Application"),
                         ("receiver", expression e1),
                         ("arg", expression e2)]

        e -> Json.string (toString e)

--type ExportSet
--  = AllExport
--  | SubsetExport (List ExportSet)
--  | FunctionExport Name
--  | TypeExport Name (Maybe ExportSet)
exports : ExportSet -> Json.Value
exports e =
    case e of
        AllExport -> Json.object [("ast_type", Json.string "ExportAll")]
        SubsetExport es -> jmap exports es
        FunctionExport n ->
            Json.object [("ast_type", Json.string "ExportFunction"),
                         ("name", Json.string n)]
        e -> Json.string (toString e)


--type Statement
--  = ModuleDeclaration ModuleName ExportSet
--  | PortModuleDeclaration ModuleName ExportSet
--  | ImportStatement ModuleName (Maybe Alias) (Maybe ExportSet)
--  | TypeAliasDeclaration Type Type
--  | TypeDeclaration Type (List Type)
--  | PortTypeDeclaration Name Type
--  | PortDeclaration Name (List Name) Expression
--  | FunctionTypeDeclaration Name Type
--  | FunctionDeclaration Name (List Name) Expression
--  | InfixDeclaration Assoc Int Name
--  | Comment String

statement : Statement -> Json.Value
statement s =
    case s of
        ModuleDeclaration n e ->
            Json.object [("ast_type", Json.string "ModuleDecl"),
                         ("name", jmap Json.string n),
                         ("exports", exports e)]

        FunctionTypeDeclaration name typ ->
            Json.object [("ast_type", Json.string "FuncType"),
                         ("name", Json.string name),
                         ("type", elmtype typ)]

        FunctionDeclaration name args expr ->
            Json.object [("ast_type", Json.string "FuncDecl"),
                         ("name", Json.string name),
                         ("args", jmap Json.string args),
                         ("expr", expression expr)]

        s -> Json.string (toString s)
