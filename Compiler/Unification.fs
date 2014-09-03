// This file contains an implementation of a type unification algorithm, which can also unify stack types.
module Compiler.Unification
open Compiler.Syntax
open Compiler

exception TypeError of string

let rec unify (constraints : List<Type * Type>) (stackConstraints : List<StackType * StackType>) : Substitution.Substitution =
    match stackConstraints with
    | ({ topElements = []; rowVariable = x1 }, { topElements = []; rowVariable = x2 })::cs when x1 = x2 -> 
        unify constraints cs
    | ({ topElements = []; rowVariable = x }, s)::cs | (s, { topElements = []; rowVariable = x })::cs ->
        let free = Free.inStack s
        if List.exists (fun x' -> x' = x) free.stackVariables then raise (TypeError ("s" + x.ToString() + " occurs in " + prettyStack s))
        let substitution = Substitution.addStack x s Substitution.empty
        Substitution.addStack x s (unify' substitution constraints cs)
    | ({ topElements = t1::ts1; rowVariable = x1 }, { topElements = t2::ts2; rowVariable = x2 })::cs ->
        unify ((t1, t2)::constraints) (({ topElements = ts1; rowVariable = x1 }, { topElements = ts2; rowVariable = x2 })::cs)
    | [] ->
        match constraints with
        | [] -> Substitution.empty
        | (t1, t2)::cs when t1 = t2 -> unify cs stackConstraints
        | (Variable x, t)::cs | (t, Variable x)::cs -> 
            let free = Free.inType t
            if List.exists (fun x' -> x' = x) free.typeVariables then raise (TypeError ("t" + x.ToString() + " occurs in " + prettyType t))
            let substitution = Substitution.addType x t Substitution.empty
            Substitution.addType x t (unify' substitution cs stackConstraints)
        | (Function (s1, s1'), Function (s2, s2'))::cs -> unify cs ((s1, s2)::(s1', s2')::stackConstraints)
        | (t1, t2)::_ -> raise (TypeError (prettyType t1 + " != " + prettyType t2))

and private unify' (substitution : Substitution.Substitution) (constraints : List<Type * Type>) (stackConstraints : List<StackType * StackType>) : Substitution.Substitution =
    let constraints' = List.map (fun (t1, t2) -> (Substitution.inType substitution t1, Substitution.inType substitution t2)) constraints
    let stackConstraints' = List.map (fun (s1, s2) -> (Substitution.inStack substitution s1, Substitution.inStack substitution s2)) stackConstraints
    unify constraints' stackConstraints'
