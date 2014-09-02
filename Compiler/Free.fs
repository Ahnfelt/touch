// This file contains logic for finding free type & stack variables.
module Compiler.Free
open System
open Compiler.Syntax


type Free = { stackVariables : List<int>; typeVariables : List<int> }
let private freeEmpty = { stackVariables = []; typeVariables = [] }
let private freeUnion' free1 free2 = { stackVariables = List.append free1.stackVariables free2.stackVariables; typeVariables = List.append free1.typeVariables free2.typeVariables }

let rec private freeInStack' (s : StackType) : Free = 
    let frees = List.map freeInType' s.topElements
    let ss = List.concat (List.map (fun free -> free.stackVariables) frees)
    let ts = List.concat (List.map (fun free -> free.typeVariables) frees)
    { stackVariables = s.rowVariable :: ss; typeVariables = ts }

and private freeInType' (t : Type) : Free =
    match t with
    | Function (s1, s2) -> freeUnion' (freeInStack' s1) (freeInStack' s2)
    | Variable x -> freeEmpty
    | Bool -> freeEmpty
    | Number -> freeEmpty
    | Text -> freeEmpty

let private freeDistinct free = { stackVariables = Seq.distinct free.stackVariables |> Seq.toList; typeVariables = Seq.distinct free.typeVariables |> Seq.toList }

let union free1 free2 = freeDistinct (freeUnion' free1 free2)
let inStack s = freeDistinct (freeInStack' s)
let inType t = freeDistinct (freeInType' t)
