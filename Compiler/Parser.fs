module Compiler.Parser
open Compiler.Syntax

(*
open FParsec

type Parser<'t> = Parser<'t, unit>

let parseType, parseTypeRef = createParserForwardedToRef<Type, unit>()

let token (x : string) : Parser<unit> = attempt (skipString x) .>> spaces

let betweenTokens (before : string) (after : string) (parser : Parser<'t>) : Parser<'t> = between (token before) (token after) parser

let identifierToken = many1Chars2 asciiLetter (asciiLetter <|> digit) .>> spaces

let stackVariableToken : Parser<int> = skipString "s" >>. many1Chars digit .>> spaces |>> fun x -> System.Int32.Parse(x)

let typeVariableToken : Parser<Type> = skipString "t" >>. many1Chars digit .>> spaces |>> fun x -> Variable (System.Int32.Parse(x))

let parseSymbol : Parser<Symbol> =
    ((attempt (identifierToken .>> token ".") .>>. (identifierToken .>> token ".")) .>>. identifierToken) |>>
    fun ((u, p), x) -> {user = u; package = p; name = x}

let parseTypeConstructor = 
    (parseSymbol .>>. (betweenTokens "[" "]" (sepBy1 parseType (token ",")))) |>>
    fun (x, ts) -> Constructor (x, ts)

let parseTypeFunction = 
    let parseSide : Parser<int * List<Type>> = (stackVariableToken <|>% 1) .>>. many parseType
    (betweenTokens "{" "}" ((parseSide .>> token "->") .>>. parseSide)) |>>
    fun ((s1, ts1), (s2, ts2)) -> Function ({topElements = List.rev ts1; rowVariable = s1}, {topElements = List.rev ts2; rowVariable = s2})

let parseTypePrimitive = choice [token "Bool" >>% Syntax.Bool; token "Number" >>% Syntax.Number; token "Text" >>% Syntax.Text]

do parseTypeRef := choice [parseTypePrimitive; typeVariableToken; parseTypeConstructor; parseTypeFunction]

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let testTypeParser str = test (parseType .>> eof) str
*)
