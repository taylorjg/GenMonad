open FsCheck
open Gen
open System

[<EntryPoint>]
let main _ = 
    let g = gen {
        let! s = Arb.generate<string> |> suchThat (String.IsNullOrEmpty >> not)
        let! c = elements s
        return (s, c)
    }
    sample 50 10 g |> Seq.iter (printfn "%A")
    0
