// This file contains logic for generating type & stack constraints from expressions.
module Compiler.Check
open System
open Operators
open Compiler.Syntax
open Compiler

type CheckerState = {
    instructions : List<Symbol * StackType * StackType>;
    environment : List<string * Type>;
    constraints : List<Type * Type>;
    stackConstraints : List<StackType * StackType>;
    nextFresh : int
}

let emptyCheckerState = {
    instructions = [];
    environment = [];
    constraints = [];
    stackConstraints = [];
    nextFresh = 0
}

type Checker(initial : CheckerState) = 
    let mutable state = initial
    member this.State = state
    member this.Fresh() =
        let fresh = state.nextFresh
        state <- { state with nextFresh = state.nextFresh + 1 }
        fresh
    member this.Constraint(t1 : Type, t2 : Type) =
        state <- { state with constraints = (t1, t2) :: state.constraints }
    member this.StackConstraint(s1 : StackType, s2 : StackType) =
        state <- { state with stackConstraints = (s1, s2) :: state.stackConstraints }
    member this.Bind(x : string, t : Type) =
        state <- { state with environment = (x, t) :: state.environment }
    member this.Lookup(x : string) : Option<Type> =
        Option.map (fun (_, t) -> t) (List.tryFind (fun (x', _) -> x' = x) state.environment)
    member this.Local<'A>(f : unit -> 'A) =
        let environment = state.environment
        let result = f ()
        state <- { state with environment = environment }
        result
    member this.Instruction(symbol : Symbol) : Option<StackType * StackType> =
        Option.map (fun (_, s1, s2) -> (s1, s2)) (List.tryFind (fun (symbol', _, _) -> symbol' = symbol) state.instructions)


let stackVariable s = { topElements = []; rowVariable = s }
let stackPush s t = { s with topElements = t :: s.topElements }


// Instantiate all the free type variables in the type, as if there were implicit "foralls" for these at the top level of the type.
let instantiateFree (checker : Checker) (t : Type) : Type =
    let free = Free.inType t
    let substitution = {
        Substitution.stacks = Map.ofList (List.map (fun x -> (x, stackVariable (checker.Fresh()))) free.stackVariables);
        Substitution.types = Map.ofList (List.map (fun x -> (x, Variable (checker.Fresh()))) free.typeVariables)
    }
    Substitution.inType substitution t


let rec checkTerm (checker : Checker) (stack : StackType) (term : Term) : StackType = 
    match term with
    | Quote terms ->
        let s = stackVariable (checker.Fresh())
        let t = checker.Local(fun () -> Function (s, checkTerms checker s terms))
        stackPush stack t
    | Unquote -> 
        let s1 = stackVariable (checker.Fresh())
        let s2 = stackVariable (checker.Fresh())
        let s = stackPush s1 (Function (s1, s2))
        checker.StackConstraint(stack, s)
        s2
    | Pop x -> 
        let t = Variable (checker.Fresh())
        checker.Bind(x, t)
        let s = stackVariable (checker.Fresh())
        checker.StackConstraint(stack, stackPush s t)
        s
    | Push x -> 
        match checker.Lookup(x) with
        | None -> raise (TypeError ("Uknown variable: " + x))
        | Some t -> stackPush stack t
    | BoolLiteral value -> stackPush stack Bool
    | NumberLiteral value -> stackPush stack Number
    | TextLiteral value -> stackPush stack Text
    | JavaScript (t, _) -> // Note that there's an implicit forall around t for all free type variables
        stackPush stack (instantiateFree checker t)
    | Instruction symbol ->
        match checker.Instruction(symbol) with
        | None -> raise (TypeError ("Uknown instruction: " + prettySymbol symbol))
        | Some (s1, s2) ->
            match instantiateFree checker (Function (s1, s2)) with
            | Function (s1', s2') ->
                checker.StackConstraint(stack, s1')
                s2'
            | _ -> raise (TypeError "Internal error: Unexpected result of instantiation")

and checkTerms (checker : Checker) (stack : StackType) (terms : List<Term>) : StackType = 
    match terms with
    | [] -> stack
    | (e::es) -> 
        let s = checkTerm checker stack e
        checkTerms checker s es
