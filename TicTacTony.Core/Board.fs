namespace TicTacTony.Core

open System
open Positions
open Helpers
open Option
open Moves
open Player

type Board = private | Empty | Has of Moves

module Board =
  
  let private _winner = function
    | [| Some a; Some b; Some c |] -> if (a = b && b = c) then Some a else None
    | _ -> None

  let internal winner = function
    | Empty -> None
    | Has moves ->
      seq [
        [| NW;  N; NE |]; [|  W;  C;  E |]; [| SW;  S; SE |];
        [| NW;  W; SW |]; [|  N;  C;  S |]; [| NE;  E; SE |];
        [| NW;  C; SE |]; [| NE;  C; SW |]
      ]
      |> Seq.map ((Array.map (flip playerAt moves)) >> _winner)
      |> Seq.choose id
      |> Seq.tryHead

  let internal player = function | Empty -> X | Has moves -> if count moves % 2 <> 1 then X else O

  let internal moves = function | Empty -> None | Has moves -> Some moves

  let internal isFull = moves >> map count >> map ((=) 9) >> defaultValue false

  let internal isWon = winner >> isSome

  let internal unoccupied = function
    | Empty -> all
    | Has moves -> let occupied = flip Seq.contains (positions moves) in all |> Seq.filter (not << occupied)

  let internal playerAt position = function | Empty -> None | Has moves -> playerAt position moves

  let toString board =
    let player = match board with | Empty -> k None | Has moves -> flip Moves.playerAt moves
    let rows = all |> Seq.chunkBySize 3 |> Seq.map (Seq.map player)
    in String.Join ("\n", Seq.map (fun r -> String.Join(" ", Seq.map (map toString >> defaultValue "_") r)) rows)
