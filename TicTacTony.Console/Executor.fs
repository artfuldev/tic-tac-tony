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
        let retry _ = print "Failed move. Try another command."
        let g x = x :> IGame
        in
            match command with
            | Exit -> 0
            | New -> Game.NewGame |> next
            | Play (x, p) -> move x p |> map g |> defaultWith retry |> next
            | PlayerAt (x, g) -> playerAt x g |> player |> print |> next
            | IsDraw f -> (if isDraw f then "Yes" else "No") |> print |> next
            | WhoWon o -> o |> whoWon |> player |> print |> next
            | TakeBack u -> u |> undo |> next

    let rec play (game: IGame) =
        let _ = game |> toString |> printfn "\n%s" 
        in s (handle play) read game
