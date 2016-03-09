open FsCheck
open Gen
open System

[<EntryPoint>]
let main _ = 

    let g1 = gen {
        let! s = Arb.generate<string> |> suchThat (String.IsNullOrEmpty >> not)
        let! c = elements s
        return (s, c)
    }
    sample 50 10 g1 |> Seq.iter (printfn "%A")

    printfn "%s" (new string('-', 80))

    let g2 = gen {
        let! s = Arb.generate<string> |> suchThat (String.IsNullOrEmpty >> not)
        return! elements s |> map (fun c -> (s, c))
    }
    sample 50 10 g2 |> Seq.iter (printfn "%A")

    0
