﻿module Tests

//language := "fsharp" // needed to run the tests

//let testTokenizer () =
//    let data =
//        [
//         "afaf(** afafa **) afafa", [Text "afaf"; OpenComment; Text " afafa "; CloseComment; Text " afafa"]
//         "(** aaf  (** afaf **) faf **)", [OpenComment; Text " aaf  "; OpenComment; Text " afaf "; CloseComment;Text " faf "; CloseComment]
//         "", []
//         "(****)", [OpenComment; CloseComment]
//         "fafdaf", [Text "fafdaf"]
//        ]
//    let result = data |> List.forall (fun (t, r) -> r = tokenize t)
//    printfn "%A" result
//    ()

//let testParser () =
//    let data =
//        [
//         "afaf(** afafa **) afafa", [CodeChunk [Text "afaf"]; NarrativeChunk [Text " afafa "]; CodeChunk [Text " afafa"]]
//         "(** aaf   afaf  faf **)",[NarrativeChunk [Text " aaf   afaf  faf "]]
//         "", []
//         "(****)", [NarrativeChunk []]
//         "fafdaf", [CodeChunk [Text "fafdaf"]]
//        ]
//    let result = data |> List.forall (fun (t, r) -> r = parse(tokenize t))
//    printfn "%A" result
//    ()

//let testBlockize () =
//    let data =
//        [
//         "afaf(** afafa **) afafa", [Code "afaf"; Narrative " afafa "; Code " afafa"]
//         "(** aaf   afaf  faf **)", [Narrative " aaf   afaf  faf "]
//         "(****)", [Narrative ""]
//         "fafdaf", [Code "fafdaf"]
//         "", []
//         "afadf afafa (** afaf **)", [Code  "afadf afafa "; Narrative " afaf "]
//        ]
//    let result = data |> List.forall (fun (t, r) -> r = !blockize t)
//    printfn "%A" result
//    ()

