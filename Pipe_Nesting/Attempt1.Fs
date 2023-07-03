module Attempt1
   open System.IO
   
   let lookUpTable =      
      let filePath = "pipe_nesting.csv"
      let rows = File.ReadLines filePath |> Seq.toList      
      let table = rows |> List.map (fun row -> row.Split ";" |> Array.toList) |> List.map (List.map int)
      let mutable lookUp = Map.empty           
      for i in 1..(table.Length - 1) do
         for j in 1..(table.Length - 1) do
            let key = (table[i][0], table[0][j])            
            let value = table[i][j]
            lookUp <- lookUp.Add (key,value)
      lookUp
   
   type Node = {
      Diameter : int
      Children : List<Node>
   }

   (*let nesting =
      {
         Diameter = 100
         Children = [
            {
               Diameter = 40
               Children = [
                  {
                     Diameter = 16
                     Children = []
                  }
               ]
            }
            {
               Diameter = 40
               Children = []
            }
         ]
      }*)
   
   let lookup s v =
         match lookUpTable.TryFind (s, v) with
         | Some n -> n
         | None -> -1   
     
   let nextNode nodesWithoutParents =  
      nodesWithoutParents |> Map.toList |> List.sortBy fst |> List.head
   
   let getChildNodes n diameter tree =
      tree |> List.filter (fun node -> node.Diameter = diameter) |> List.truncate n 

   let findBiggerPipe pipeList smallerPipeSize =
      let biggerPipes = pipeList |> Map.filter (fun k v -> lookup smallerPipeSize k > 1) 
      match biggerPipes with 
         | m when (m = Map.empty) -> None
         | _ -> Some (biggerPipes |> Map.toList |> List.minBy fst)
      
   let decrementMapValue key n =
      Map.change key (fun x ->
                        match x with
                        | Some x -> Some (x - n)
                        | None -> None) 
      
   let nestPipes (input : Map<int, int>) =

      let (startPipeDiameter, count) = input |> Map.toList |> List.minBy snd
      let mutable tree = List.empty
      
      for i in 0..count do
         tree <- {Diameter = startPipeDiameter ; Children = List.empty}::tree
    
      let nodesWithoutParents = Map.empty |> Map.add startPipeDiameter count
      let pipesList = input |> Map.remove startPipeDiameter
            
      let rec inner tree pipeList nodesWithoutParents =
         if pipeList = Map.empty then tree
         else
            let diameter, count = nextNode nodesWithoutParents    
            let biggerPipe = findBiggerPipe pipeList diameter
            
            if (biggerPipe = None) then
               let newNodesWithoutParents = nodesWithoutParents |> decrementMapValue diameter count
               inner tree pipeList newNodesWithoutParents   
            else 
               let Diameter =
                  match biggerPipe with
                  | None -> 0
                  | Some v -> v |> fst
               let n = lookup diameter Diameter
               let children =  getChildNodes n diameter tree
   
               let newTree = {Diameter = Diameter ; Children = children} :: tree
               let newPipeList = pipeList |> decrementMapValue Diameter 1
               let newNodesWithoutParents = nodesWithoutParents |> decrementMapValue diameter (children.Length)
      
               inner newTree newPipeList newNodesWithoutParents
                 
      let answer = inner tree pipesList nodesWithoutParents
      printfn $"{answer[0]}"