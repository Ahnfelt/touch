// This algorithm is used for finding the proper compile order and mutually recursive functions.
module Compiler.Tarjan
// Produces strongly connected components, reverse topologically sorted
// Based on Tarjan's SCC algorithm: http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
let stronglyConnectedComponents (vertices : 'v list) (edges : 'v -> 'v list) : 'v list list =
    
    let index = ref 0
    let s = ref []
    let components = ref []
    let indexMap = ref Map.empty
    let lowlinkMap = ref Map.empty

    let rec connect (v : 'v) =
        indexMap := Map.add v !index !indexMap
        lowlinkMap := Map.add v !index !lowlinkMap
        index := !index + 1
        s := v :: !s
        for w in edges v do
            if not (Map.containsKey w !indexMap) then 
                connect w
                lowlinkMap := Map.add v (min (Map.find v !lowlinkMap) (Map.find w !lowlinkMap)) !lowlinkMap
            else if List.exists (fun w' -> w = w') !s then
                lowlinkMap := Map.add v (min (Map.find v !lowlinkMap) (Map.find w !indexMap)) !lowlinkMap
        if (Map.find v !lowlinkMap) = (Map.find v !indexMap) then
            let i = List.findIndex (fun v' -> v = v') !s
            components := (Seq.toList <| Seq.take (i + 1) !s) :: !components
            s := Seq.toList <| Seq.skip (i + 1) !s

    for v in vertices do
        if not (Map.containsKey v !indexMap) then connect v

    List.rev !components
    

(*
algorithm tarjan is
  input: graph G = (V, E)
  output: set of strongly connected components (sets of vertices)

  index := 0
  S := empty
  for each v in V do
    if (v.index is undefined) then
      strongconnect(v)
    end if
  end for

  function strongconnect(v)
    // Set the depth index for v to the smallest unused index
    v.index := index
    v.lowlink := index
    index := index + 1
    S.push(v)

    // Consider successors of v
    for each (v, w) in E do
      if (w.index is undefined) then
        // Successor w has not yet been visited; recurse on it
        strongconnect(w)
        v.lowlink  := min(v.lowlink, w.lowlink)
      else if (w is in S) then
        // Successor w is in stack S and hence in the current SCC
        v.lowlink  := min(v.lowlink, w.index)
      end if
    end for

    // If v is a root node, pop the stack and generate an SCC
    if (v.lowlink = v.index) then
      start a new strongly connected component
      repeat
        w := S.pop()
        add w to current strongly connected component
      until (w = v)
      output the current strongly connected component
    end if
  end function
*)
