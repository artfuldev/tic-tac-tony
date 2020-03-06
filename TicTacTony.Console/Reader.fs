namespace TicTacTony.Console

open Parser
open System
open TicTacTony.Core
open Helpers

module Reader =

    let read game =
        let parse = parse game
        let _ =
            [ Position.all |> Seq.map (string >> sprintf "M %s")
            ; Position.all |> Seq.map (string >> sprintf "P %s")
            ; seq ["I"; "W"; "T"; "N"; "E"]
            ] |> Seq.concat |> Seq.map parse |> Seq.choose id
            |> Seq.map Commands.toString
            |> Seq.append (Seq.singleton "Choose from:")
            |> Seq.iter (printfn "%s")
        let rec read parse =
            let _ = printfn "%s" "Enter your choice here, without []:"
            in
                match Console.ReadLine () |> parse with
                | None -> let _ = printfn "invalid, try again" in read parse
                | Some c -> c |> Commands.toDescription |> printfn "%s" |> k c
        in read parse
