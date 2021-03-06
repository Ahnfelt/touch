// This file contains logic for applying substitutions for type & stack variables.
module Compiler.Substitution
open System
open Compiler.Syntax
open Compiler.Free


type Substitution = { stacks : Map<int, StackType>; types : Map<int, Type> }

let empty = { stacks = Map.empty; types = Map.empty }


// Together with a deterministic algorithm for finding free variables, this subtitution
// provides a canonical renaming such that if two types are "equal modulo renaming of free type variables",
// then their renaming is syntactically equal.
let renaming free = { 
    stacks = Map.ofList <| List.mapi (fun i x -> (x, { topElements = []; rowVariable = (i + 1) })) free.stackVariables; 
    types = Map.ofList <| List.mapi (fun i x -> (x, Variable (i + 1))) free.typeVariables;
}

// NOTE: The left substitution wins. This is slow if the left substitution is big.
let union substitution1 substitution2 = { 
    stacks = Map.fold (fun map x s -> Map.add x s map) substitution2.stacks substitution1.stacks;
    types = Map.fold (fun map x t -> Map.add x t map) substitution2.types substitution1.types
}

let addStack x s substitution = { substitution with stacks = Map.add x s substitution.stacks }
let addType x t substitution = { substitution with types = Map.add x t substitution.types }

let rec inStack (substitution : Substitution) (stack : StackType) : StackType =
    let ts = List.map (inType substitution) stack.topElements
    match Map.tryFind stack.rowVariable substitution.stacks with
    | Some(s) -> { topElements = List.append ts s.topElements; rowVariable = s.rowVariable }
    | None -> { stack with topElements = ts }
    
and inType (substitution : Substitution) (t : Type) : Type =
    match t with
    | Function (s1, s2) -> Function (inStack substitution s1, inStack substitution s2)
    | Constructor (x, ts) -> Constructor (x, List.map (inType substitution) ts)
    | Variable x -> 
        match Map.tryFind x substitution.types with
        | Some(t2) -> t2
        | None -> t
    | Bool -> Bool
    | Number -> Number
    | Text -> Text


let pretty substitution =
    let stacks = List.map (fun (x, s) -> "s" + x.ToString() + " = " + prettyStack s) (Map.toList substitution.stacks)
    let types = List.map (fun (x, t) -> "t" + x.ToString() + " = " + prettyType t) (Map.toList substitution.types)
    "[" + (String.concat ", " (List.append stacks types)) + "]"
