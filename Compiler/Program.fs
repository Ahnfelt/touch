// This file is currently only here for ad-hoc tests and to keep the compiler happy.
module Compiler.Program
open Compiler
open Compiler.Syntax
open Compiler.Tarjan
open Compiler.Check

// In the following examples, s and s0-s99 are stack variables, while t and t0-t99 are type variables.

let es0 = [] // (s -> s)
let es1 = [Pop "x"; Push "x"; Push "x"] // (s t -> s t t)
let es2 = [NumberLiteral 3.0; Pop "x"; Push "x"; Push "x"] // (s -> s Number Number)
let es3 = [Pop "x"; Quote [Push "x"; NumberLiteral 3.0]] // (s t -> s (s' -> s' t Number))
let es4 = [Pop "x"; Quote [Push "x"; NumberLiteral 3.0]; Unquote] // (s t -> s t Number)

let testCheck es =
    let t = Function <| Typing.checkFunction [] es
    printfn "%s" (Syntax.prettyType t)

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

let testFib () =
    let core x = { user = "touch"; package = "core"; name = x }
    let name x = { user = "touch"; package = "test"; name = x }
    let predefinedInstructionTypes = [
            (core ">=", stackPush (stackPush (stackVariable 1) Number) Number, stackPush (stackVariable 1) Bool);
            (core "+", stackPush (stackPush (stackVariable 1) Number) Number, stackPush (stackVariable 1) Number); 
            (core "-", stackPush (stackPush (stackVariable 1) Number) Number, stackPush (stackVariable 1) Number);
            (core "if", stackPush (stackPush (stackPush (stackVariable 1) Bool) (Function (stackVariable 1, stackVariable 2))) (Function (stackVariable 1, stackVariable 2)), stackVariable 2);
            (name "fib'", stackPush (stackVariable 1) Number, stackPush (stackVariable 1) Number); 
        ]
    let i x = Instruction (core x)
    let instructions = [
            (name "fib", [
                Pop "n"; 
                Push "n"; NumberLiteral 2.0; i ">=";
                Quote [
                    Push "n"; NumberLiteral 1.0; i "-"; Instruction (name "fib");
                    Push "n"; NumberLiteral 2.0; i "-"; Instruction (name "fib");
                    i "+"
                ];
                Quote [NumberLiteral 1.0];
                i "if"
            ])
        ]
    let instructionTypes = Typing.checkInstructions predefinedInstructionTypes instructions
    for (x, s1, s2) in instructionTypes do 
        printfn "%s : %s" (Syntax.prettySymbol x) (Syntax.prettyType (Function (s1, s2)))
    

[<EntryPoint>]
let main argv = 
    //testTarjan ()
    //testCheck es3
    testFib ()
    //printfn "Press return to continue..."
    //System.Console.Read() |> ignore
    0
