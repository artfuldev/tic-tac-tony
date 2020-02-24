namespace TicTacTony.Console

open Parser
open System
open TicTacTony.Core.Game


module Reader =

    let read game =
        let parse = parse game
        let _ =
            [ positions |> Seq.map (string >> sprintf "M %s")
            ; positions |> Seq.map (string >> sprintf "P %s")
            ; seq ["I"; "W"; "T"; "N"; "E"]
            ] |> Seq.concat |> Seq.map parse |> Seq.choose id
            |> Seq.map Commands.toString
            |> Seq.append (Seq.singleton "Choose from:")
            |> Seq.iter (printfn "%s")
        let rec read parse =
            let _ = printfn "%s" "Enter your choice here, without []:" in
            match Console.ReadLine () |> parse with
            | None -> let _ = printfn "invalid, try again" in read parse
            | Some command -> command
        in read parse

