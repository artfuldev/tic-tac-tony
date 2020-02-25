namespace TicTacTony.Console

open TicTacTony.Core
open TicTacTony.Console
open Reader
open Helpers


module Executor =

    let private player = Option.map string >> Option.defaultValue "Nobody"

    let private handle next game command =
        let _ = command |> Commands.toDescription |> printfn "%s"
        let print = printfn "%s" >> k game
        in
            match command with
            | Exit -> 0
            | New -> Game.NewGame |> next
            | Play (m, p) -> p.Move m |> next
            | PlayerAt (x, g) -> g.PlayerAt x |> player |> print |> next
            | IsDraw f -> (if f.IsDraw() then "Yes" else "No") |> print |> next
            | WhoWon o -> o.WhoWon() |> player |> print |> next
            | TakeBack u -> u.TakeBack () |> next

    let rec play game =
        match game with
        | Fresh (g, _) | Played (g, _, _) | Won (g, _, _, _)
        | Drawn (g, _, _, _) ->
            let _ = g.Board |> Board.toString |> printfn "\n%s"
            in s (handle play) read game
