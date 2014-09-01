module Compiler.Check
open System
open Compiler.Syntax

type Checker<'A> = {
    environment : List<String * Type>;
    constraints : List<Type * Type>;
    stackConstraints : List<StackType * StackType>;
    result : 'A
}

type CheckerBuilder() =
    member c.Bind(m, f) = 
        let m' = f m.result
        {
            environment = List.append m'.environment m.environment;
            constraints = List.append m'.constraints m.constraints;
            stackConstraints = List.append m'.stackConstraints m.stackConstraints;
            result = m'.result
        }
    member c.Return(x) =
        { 
            environment = [];
            constraints = [];
            stackConstraints = [];
            result = x
        }
    member c.ReturnFrom(m) = m

let checker = new CheckerBuilder()


// Maybe the type should be actually Term * StackType -> StackType
let rec checkTerm (term : Term) : Checker<Type> = 
    match term with
    | Quote terms -> checker { return! checkTerms terms }
    | Unquote -> checker { return Bool }
    | Pop variable -> checker { return Variable "todo: fresh" }
    | Push variable -> checker { return Variable "todo: lookup" }
    | BoolLiteral value -> checker { return Bool }
    | NumberLiteral value -> checker { return Number }
    | TextLiteral value -> checker { return Text }
    | Instruction symbol -> checker { return Text }


and checkTerms (terms : List<Term>) : Checker<Type> = 
    match terms with
    | [] -> checker { return Text }
    | (e::es) -> checkTerms es
