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
    | TextLiteral of double
    | Instruction of Symbol

type StackType = { rowVariable : string; topElements : List<Type> }

type Type
    = Function of StackType * StackType
    | Variable of string
    | Bool
    | Number
    | Text




// TODO: Product & sum type (de)construction
// The following type cases
//    | SumType of Symbol
//    | ProductType of Symbol
