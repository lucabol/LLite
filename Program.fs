let fp = ref (Unchecked.defaultof<float -> float>)

let fwdDecl<'a> () = ref Unchecked.defaultof<'a>

let takeTurns = fwdDecl<float -> float> ()

takeTurns := fun x -> x + 1.

takeTurns := fun x -> x + 3.

!takeTurns 3.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
