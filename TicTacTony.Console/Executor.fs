namespace TicTacTony.Console

open TicTacTony.Core
open TicTacTony.Console
open Reader
open Helpers
open Game
open Option


module Executor =

    let private player = map string >> defaultValue "Nobody"

    let private handle (next: IGame -> int) (game: IGame) command =
        let _ = command |> Commands.toDescription |> printfn "%s"
        let print = printfn "%s" >> k game
        in
            match command with
            | Exit -> 0
            | New -> Game.NewGame |> next
            | Play (x, p) -> p.Move x |> next
            | PlayerAt (x, g) -> g.PlayerAt x |> player |> print |> next
            | IsDraw f -> (if f.IsDraw () then "Yes" else "No") |> print |> next
            | WhoWon o -> o.WhoWon () |> player |> print |> next
            | TakeBack u -> u.Undo () |> next

    let rec play (game: IGame) =
        let _ = game |> toString |> printfn "\n%s"
        in s (handle play) read game
