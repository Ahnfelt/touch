// This file is currently only here to keep the compiler happy.
module Compiler.Program
open Compiler
open Compiler.Syntax


let es1 = [Pop "x"; Push "x"; Push "x"] // (s* t -> s* t t)
let es2 = [NumberLiteral 3.0; Pop "x"; Push "x"; Push "x"] // (s* -> s* Number Number)

let check es = 
    let checker = new Check.Checker(Check.emptyCheckerState)
    let s1 = Check.stackVariable -1
    let s2 = Check.checkTerms checker s1 es
    let substitution = Unification.unify checker.State.constraints checker.State.stackConstraints
    Substitution.inType substitution (Function (s1, s2))

[<EntryPoint>]
let main argv = 
    let t = check es2
    printfn "%s" (Syntax.prettyType t)
    System.Console.Read() |> ignore
    0

