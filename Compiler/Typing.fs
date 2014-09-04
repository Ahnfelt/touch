module Compiler.Typing
open Compiler.Syntax
open Compiler

let rec private referencedInstructions' e =
    match e with
    | Quote es -> List.map referencedInstructions' es |> List.concat
    | Instruction x -> [x]
    | _ -> []
    
and private referencedInstructions es = List.map referencedInstructions' es |> List.concat |> Seq.distinct |> Seq.toList


let checkFunction instructionTypes es = 
    let checker = new Check.Checker({Check.emptyCheckerState with nextFresh = 1; instructions = instructionTypes})
    let s1 = Check.stackVariable 0
    let s2 = Check.checkTerms checker s1 es
    let substitution = Unification.unify checker.State.constraints checker.State.stackConstraints
    let rec iterate f x = // TODO: How to avoid doing this?
        let x' = f x
        if x' = x then x else iterate f x'
    let s1' = iterate (Substitution.inStack substitution) s1
    let s2' = iterate (Substitution.inStack substitution) s2
    let renaming = Substitution.renaming <| Free.inType (Function (s1', s2'))
    (Substitution.inStack renaming s1', Substitution.inStack renaming s2')


let rec checkComponents instructionTypes components =
    match components with
    | [] -> List.rev instructionTypes
    | (instructions::components') ->
        // This is a probably horribly broken hack to work around polymorphic recursion: 
        // - Infer types for the instructions where recursive occurances are assumed to be (forall s1 s2. s1 -> s2)
        // - Then check the instructions again with the infered types
        // - Finally check that the newly inferred type is the same as the type initially inferred
        let instructionTypes' = List.append (List.map (fun (x, _) -> (x, Check.stackVariable 1, Check.stackVariable 2)) instructions) instructionTypes
        let instructionTypes'' = List.map (fun (x, es) -> (x, checkFunction instructionTypes' es)) instructions
        let instructionTypes''' = List.append (List.map (fun (x, (s1, s2)) -> (x, s1, s2)) instructionTypes'') instructionTypes
        for (x, es) in instructions do 
            let (_, s1, s2) = List.find (fun (x', _, _) -> x' = x) instructionTypes'''
            let (s1', s2') = checkFunction instructionTypes''' es 
            let (t, t') = (Function (s1, s2), Function (s1', s2'))
            // The syntactic equivalence is enough here only because of the canonical renaming done in checkFunction.
            if t <> t' then raise (Unification.TypeError ("Recursive function type " + prettyType t + " != " + prettyType t'))
        checkComponents instructionTypes''' components'
    

let checkInstructions (predefinedInstructionTypes : (Symbol * StackType * StackType) list) (instructions : (Symbol * Term list) list) : (Symbol * StackType * StackType) list =
    // Group by mutual recursion and check in order of build dependency
    let instructionMap = Map.ofList instructions
    let notPredefined x = not (List.exists (fun (x2, _, _) -> x = x2) predefinedInstructionTypes)
    let referencedInstructions' es = referencedInstructions es |> List.filter notPredefined
    let references = List.map (fun (x, es) -> (x, referencedInstructions' es)) instructions
    let edges = references |> Map.ofList
    let vertices = List.append (List.map fst instructions) (List.map snd references |> List.concat) |> Seq.distinct |> Seq.toList
    let components = Tarjan.stronglyConnectedComponents vertices (fun v -> Map.find v edges)
    let components' = List.map (List.map (fun x -> (x, Map.find x instructionMap))) components
    checkComponents predefinedInstructionTypes components'
