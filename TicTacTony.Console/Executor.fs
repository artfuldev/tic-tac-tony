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
        let string =
            match game with
            | Fresh game -> Game.ToString game
            | Played (Played1 game) -> Game.ToString game
            | Played (Played2 game) -> Game.ToString game
            | Played (Played3 game) -> Game.ToString game
            | Played (Played4 game) -> Game.ToString game
            | Played (Played5 game) -> Game.ToString game
            | Played (Played6 game) -> Game.ToString game
            | Played (Played7 game) -> Game.ToString game
            | Played (Played8 game) -> Game.ToString game
            | Won (After5 game) -> Game.ToString game
            | Won (After6 game) -> Game.ToString game
            | Won (After7 game) -> Game.ToString game
            | Won (After8 game) -> Game.ToString game
            | Won (After9 game) -> Game.ToString game
            | Drawn game -> Game.ToString game
        let _ = printfn "\n%s" 
        in s (handle play) read game
