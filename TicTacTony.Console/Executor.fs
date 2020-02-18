namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader
open Helpers


module Executor =

    let private player p' = p' |> map string |> defaultValue "Nobody"

    let private handle continuation game command =
        let _ = command |> Commands.toDescription |> printfn "%s"
        let print = printfn "%s" >> k game
        let game' =
            match command with
            | Exit -> exit 0
            | New -> Game.NewGame
            | Play (m, p) -> p.Move m
            | PlayerAt (x, g) -> g.PlayerAt x |> player |> print
            | IsDraw f -> (if f.IsDraw() then "Yes" else "No") |> print
            | WhoWon o -> o.WhoWon() |> player |> print
            | TakeBack u -> u.TakeBack ()
        in game' |> continuation

    let rec play game =
        match game with
        | Fresh (g, _) | Played (g, _, _) | Won (g, _, _, _)
        | Drawn (g, _, _, _) ->
            let _ = g.Board |> Board.toString |> printfn "\n%s"
            in s (handle play) read game
