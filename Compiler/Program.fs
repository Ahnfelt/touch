// This file is currently only here for ad-hoc tests and to keep the compiler happy.
module Compiler.Program
open Compiler
open Compiler.Syntax
open Compiler.Tarjan

// In the following examples, s and s0-s99 are stack variables, while t and t0-t99 are type variables.

let es0 = [] // (s -> s)
let es1 = [Pop "x"; Push "x"; Push "x"] // (s t -> s t t)
let es2 = [NumberLiteral 3.0; Pop "x"; Push "x"; Push "x"] // (s -> s Number Number)
let es3 = [Pop "x"; Quote [Push "x"; NumberLiteral 3.0]] // (s t -> s (s' -> s' t Number))
let es4 = [Pop "x"; Quote [Push "x"; NumberLiteral 3.0]; Unquote] // (s t -> s t Number)

let check es = 
    let checker = new Check.Checker({Check.emptyCheckerState with nextFresh = 1})
    let s1 = Check.stackVariable 0
    let s2 = Check.checkTerms checker s1 es
    let substitution = Unification.unify checker.State.constraints checker.State.stackConstraints
    Substitution.inType substitution (Function (s1, s2))

let testCheck es =
    let t = check es
    printfn "%s" (Syntax.prettyType t)
    printfn "Press return to continue..."

let testTarjan () =
    let vertices = ["a"; "b"; "c"; "d"; "e"; "f"]
    let edges v =
        match v with
        | "a" -> ["b"; "c"]
        | "b" -> ["a"]
        | "c" -> ["d"]
        | "d" -> ["e"; "f"]
        | "e" -> ["c"]
        | _ -> []
    let components = stronglyConnectedComponents vertices edges
    printfn "%A" components

[<EntryPoint>]
let main argv = 
    //testTarjan ()
    testCheck es3
    System.Console.Read() |> ignore
    0
