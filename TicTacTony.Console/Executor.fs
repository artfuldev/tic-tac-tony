namespace TicTacTony.Console

open TicTacTony.Core
open TicTacTony.Console
open Reader
open Helpers
open Game


module Executor =

    let private player = Option.map string >> Option.defaultValue "Nobody"

    let private print game = printfn "%s" >> k game

    let private handle next game = function
        | Exit -> 0
        | New -> NewGame |> next
        | Play move -> move |> make |> next
        | PlayerAt (x, g) -> x |> playerAt g |> player |> print game |> next
        | IsDraw f -> (if isDraw f then "Yes" else "No") |> print game |> next
        | WhoWon o -> o |> whoWon |> player |> print game |> next
        | TakeBack u -> u |> takeBack |> next

    let rec play game =
        s (handle play) (toString >> print game >> read) game
