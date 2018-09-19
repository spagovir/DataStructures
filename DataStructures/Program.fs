// Learn more about F# at http://fsharp.org

open System
open ListTreeHeap 

let rec testAddHeapWithList lth l =
    match l with 
    | i :: r -> testAddHeapWithList (add lth i) r
    | [] -> lth

[<EntryPoint>]
let main argv =
    let testList = [15; 21; 21; 9; 1; 59]
    printfn "%A" (heapsort testList)
    Console.In.ReadLine () |> ignore
    0 // return an integer exit code
