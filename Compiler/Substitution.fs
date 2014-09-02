module Compiler.Substitution
open System
open Compiler.Syntax


type Substitution = { stacks : Map<int, StackType>; types : Map<int, Type> }

let rec inStack (substitution : Substitution) (stack : StackType) : StackType =
    let ts = List.map (inType substitution) stack.topElements
    match Map.tryFind stack.rowVariable substitution.stacks with
    | Some(s) -> { topElements = List.append ts s.topElements; rowVariable = s.rowVariable }
    | None -> { stack with topElements = ts }
    
and inType (substitution : Substitution) (t : Type) : Type =
    match t with
    | Function (s1, s2) -> Function (inStack substitution s1, inStack substitution s2)
    | Variable x -> 
        match Map.tryFind x substitution.types with
        | Some(t2) -> t2
        | None -> t
    | Bool -> Bool
    | Number -> Number
    | Text -> Text
