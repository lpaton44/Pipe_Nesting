open System.IO
open Attempt1

[<EntryPoint>]
let main argv =
    let input =
      Map [(25, 3); (40, 1); (110, 1)]
    
    Attempt1.nestPipes input
    0