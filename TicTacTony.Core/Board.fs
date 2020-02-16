namespace TicTacTony.Core

open Cell
open System
open Moves

type Board =
  private 
  | Empty
  | Played of Moves

module Board =
  let private grid =
    [|
      [| NW; N; NE |]
      [| W; C; E |]
      [| SW; S; SE |]
    |]
  
  let private _winner = function
    | [| Taken a; Taken b; Taken c |] -> if (a = b && b = c) then Some a else None
    | _ -> None

  let internal winner = function
    | Empty -> None
    | Played moves ->
      seq [
        [| NW; N; NE |]
        [| W; C; E |]
        [| SW; S; SE |]
        [| NW; W; SW |]
        [| N; C; S |]
        [| NE; E; SE |]
        [| NW; C; SE |]
        [| NE; C; SW |]
      ]
      |> Seq.map ((Array.map (create moves)) >> _winner)
      |> Seq.choose id
      |> Seq.tryHead

  let internal empty =
    Empty

  let internal player = function
    | Empty -> X
    | Played moves -> if count moves % 2 <> 1 then X else O

  let internal moves = function
    | Empty -> None
    | Played moves -> Some moves

  let unoccupied = function
    | Empty -> Positions.all
    | Played moves ->
      let occupied x = List.contains x (Moves.positions moves) in Positions.all |> List.filter (not << occupied)

  let internal played moves =
    Played moves

  let private _print cells =
    String.Join ("\n", Array.map (fun row -> String.Join (" ", Array.map toString row)) cells)

  let private cells = function
    | Empty -> Array.create 3 (Array.create 3 Cell.Empty)
    | Played moves -> Array.map (Array.map (create moves)) grid

  let internal playerAt position = function
    | Empty -> None
    | Played moves -> playerAt position moves

  let print =
    cells >> _print
