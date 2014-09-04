// This file contains the AST for terms & types.
module Compiler.Syntax
open System


type Symbol = { user : string; package : string; name : string }

type Term
    = Quote of List<Term>
    | Unquote
    | Pop of string
    | Push of string
    | BoolLiteral of bool
    | NumberLiteral of double
    | TextLiteral of string
    | JavaScript of Type * string
    | Instruction of Symbol

and StackType = { topElements : List<Type>; rowVariable : int } // In reverse order of the notation, so that :: is "push"

and Type
    = Function of StackType * StackType
    | Variable of int
    | Bool
    | Number
    | Text


exception TypeError of string


// Syntactic sugar for function types
let (==>) ((s1, ts1) : int * Type list) ((s2, ts2) : int * Type list) = Function ({ topElements = List.rev ts1; rowVariable = s1 }, { topElements = List.rev ts2; rowVariable = s2 })
let (-->) (ts1 : Type list) (ts2 : Type list) = (1, ts1) ==> (1, ts2)


// TODO: Product & sum type (de)construction
// The following type cases
//    | SumType of Symbol
//    | ProductType of Symbol


let rec prettyType t =
    match t with
    | Function (s1, s2) -> "{" + prettyStack s1 + " -> " + prettyStack s2 + "}"
    | Variable x -> "t" + x.ToString()
    | Bool -> "Bool"
    | Number -> "Number"
    | Text -> "Text"

and prettyStack s =
    "s" + s.rowVariable.ToString() + String.concat "" (List.map (fun t -> " " + prettyType t) (List.rev s.topElements))


let prettySymbol x = x.user + "." + x.package + "." + x.name
