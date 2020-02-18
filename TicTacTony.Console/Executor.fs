namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader
open System

open Move
open Helpers
open Game

module Executor =

    let private moves p =
        p.Moves |> Seq.map (position >> Move >> Some)
  
    let private options game =
        [ ifPlayable (k Seq.empty) moves
        ; Board.positions |> Seq.map (PlayerAt >> Some) |> k
        ; ifFull (k Seq.empty) (k (Seq.singleton (Some IsDraw)))
        ; ifOver (k Seq.empty) (k (Seq.singleton (Some WhoWon)))
        ; ifUndoable (k Seq.empty) (k (Seq.singleton (Some TakeBack)))
        ; seq [Some New; Some Exit] |> k
        ]
        |> Seq.map (apply game)
        |> Seq.concat
        |> Seq.choose id

    let private player p' = p' |> map string |> defaultValue "Nobody"

    let private playerAt p g = g.PlayerAt p |> player

    let private whoWon o = o.WhoWon() |> player

    let private isDraw f = if f.IsDraw () then "Yes" else "No"

    let private takeBack u = u.TakeBack ()

    let private move x p = p.Moves |> Seq.find (position >> ((=) x)) |> p.Move

    let private handle continuation game command =
        let print = printfn "%s" >> k game
        let fail _ = failwith "impossible"
        let play =
            match command with
            | Exit -> fun _ -> exit 0
            | New -> k NewGame
            | Move x -> ifPlayable fail (move x)
            | PlayerAt x -> onGame (playerAt x >> print)
            | IsDraw -> ifFull fail (isDraw >> print)
            | WhoWon -> ifOver fail (whoWon >> print)
            | TakeBack -> ifUndoable fail takeBack
        in command |> Commands.toDescription |> print |> play |> continuation

    let rec play game =
        let _ = onGame (fun g -> Board.toString g.Board |> printfn "\n%s") game
        in s (handle play) (options >> read) game

    
