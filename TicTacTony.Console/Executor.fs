namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader
open Move
open Helpers
open Game


module Executor =
  
    let private options game =
        [ ifPlayable (fun p -> p.Moves |> Seq.map (position >> Move))
        ; onGame (Board.positions |> Seq.map PlayerAt |> Some |> k)
        ; ifFull (Seq.singleton IsDraw |> k)
        ; ifOver (Seq.singleton WhoWon |> k)
        ; ifUndoable (Seq.singleton TakeBack |> k)
        ; seq [New; Exit] |> Some |> k
        ]
        |> Seq.map (apply game)
        |> Seq.choose id
        |> Seq.concat

    let private player p' = p' |> map string |> defaultValue "Nobody"

    let private playerAt p g = g.PlayerAt p |> player

    let private whoWon o = o.WhoWon() |> player

    let private isDraw f = if f.IsDraw () then "Yes" else "No"

    let private takeBack u = u.TakeBack ()

    let private move x p = p.Moves |> Seq.find (position >> ((=) x)) |> p.Move

    let private handle succeed fail game command =
        let print = printfn "%s" >> k game
        let game = command |> Commands.toDescription |> print
        let play =
            match command with
            | Exit -> fun _ -> exit 0
            | New -> NewGame |> Some |> k
            | Move x -> ifPlayable (move x)
            | PlayerAt x -> onGame (playerAt x >> print >> Some)
            | IsDraw -> ifFull (isDraw >> print)
            | WhoWon -> ifOver (whoWon >> print)
            | TakeBack -> ifUndoable takeBack
        in game |> play |> defaultWith fail |> succeed

    let rec play game =
        let print = onGame (fun g -> Board.toString g.Board |> printfn "\n%s")
        let fail _ = failwith "impossible"
        in s (handle play fail) (print >> k game >> options >> read) game
