module Compiler.Emit
open Compiler.Syntax

// TODO: Check this string escaping logic
let escapeChar c = 
    match c with
    | '\"' -> "\\\""
    | '\\' -> "\\\\"
    | '\b' -> "\\\b"
    | '\f' -> "\\\f"
    | '\n' -> "\\\n"
    | '\r' -> "\\\r"
    | '\t' -> "\\\t"
    | _ -> c.ToString()
let escapeString s = "\"" + String.collect escapeChar s + "\""

// TODO: Check that this will become a valid JavaScript identifier to avoid XSS
let private jsSymbol symbol = symbol.user + "_" + symbol.package + "_" + symbol.name

let rec emit (term : Term) : string =
    match term with
    | Quote terms ->
        "$.push(function($) {\n" + 
        String.concat "" (List.map emit terms) +
        "});\n"
    | Unquote -> 
        "($.pop())($);\n"
    | Pop x -> 
        "var " + x + "_ = $.pop();\n"
    | Push x -> 
        "$.push(" + x + "_);\n"
    | BoolLiteral value -> "$.push(" + value.ToString() + ");\n"
    | NumberLiteral value -> "$.push(" + value.ToString() + ");\n"
    | TextLiteral value -> "$.push(" + escapeString value + ");\n"
    | JavaScript (_, code) -> code + ";\n"
    | Instruction symbol -> jsSymbol symbol + "($);\n"

let emitInstruction (symbol, t, terms) : string =
    "// " + prettyType t + "\n" +
    "function " + jsSymbol symbol + "($) {\n" +
    String.concat "" (List.map emit terms) +
    "}\n"

let emitInstructions (instructions : (Symbol * Type * Term list) list) : string =
    String.concat "\n" (List.map emitInstruction instructions)

let emitProgram (mainSymbol : Symbol) (instructions : (Symbol * Type * Term list) list) : string =
    emitInstructions instructions + "\n" + jsSymbol mainSymbol + "([]);"
