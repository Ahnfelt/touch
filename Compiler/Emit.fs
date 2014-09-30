module Compiler.Emit
open System
open Compiler.Syntax

// TODO: Check this string escaping logic
let escapeChar c = 
    match c with
    | '\'' -> "\\\'"
    | '\"' -> "\\\""
    | '\\' -> "\\\\"
    | '\b' -> "\\\b"
    | '\f' -> "\\\f"
    | '\n' -> "\\\n"
    | '\r' -> "\\\r"
    | '\t' -> "\\\t"
    | _ -> c.ToString()
let escapeString s = "\"" + String.collect escapeChar s + "\""

let private jsIdentifier (x : String) = 
    let escape c = if (int) c <= 0xFF then  "$" + ((int) c).ToString("X2") else "$$" + ((int) c).ToString("X4")
    let first = if Char.IsLetter(x.[0]) && (int) x.[0] < 128 then x.[0].ToString() else escape x.[0]
    let rest = String.collect (function c -> if Char.IsLetterOrDigit(c) && (int) c < 128 then c.ToString() else escape c) (x.Substring(1))
    first + rest
let private jsSymbol symbol = jsIdentifier symbol.user + "_" + jsIdentifier symbol.package + "_" + jsIdentifier symbol.name

let rec emit (term : Term) : string =
    match term with
    | Quote terms ->
        "stack.push(function(stack) {\n" + 
        String.concat "" (List.map emit terms) +
        "});\n"
    | Unquote -> 
        "(stack.pop())(stack);\n"
    | Pop x -> 
        "var " + jsIdentifier x + "_ = stack.pop();\n"
    | Push x -> 
        "stack.push(" + jsIdentifier x + "_);\n"
    | BoolLiteral value -> "stack.push(" + value.ToString() + ");\n"
    | NumberLiteral value -> "stack.push(" + value.ToString() + ");\n"
    | TextLiteral value -> "stack.push(" + escapeString value + ");\n"
    | JavaScript (_, code) -> code + ";\n"
    | Instruction symbol -> jsSymbol symbol + "(stack);\n"

let emitInstruction (symbol, t, terms) : string =
    "// " + prettyType t + "\n" +
    "function " + jsSymbol symbol + "(stack) {\n" +
    String.concat "" (List.map emit terms) +
    "}\n"

let emitInstructions (instructions : (Symbol * Type * Term list) list) : string =
    String.concat "\n" (List.map emitInstruction instructions)

let emitProgram (mainSymbol : Symbol) (instructions : (Symbol * Type * Term list) list) : string =
    emitInstructions instructions + "\n" + jsSymbol mainSymbol + "([]);"
