namespace TicTacTony.Core

open Board
open Moves
open Move

module Runner =
  
  let newGame =
    XToMove (empty, [New])

  let withMoves moves =
    GameDrawn (played moves, [New])

  let run = function
    | New -> newGame
    | Play (move, board) -> board |> moves |> make move |> withMoves
    | TakeBack moves -> match undo moves with | None -> newGame | Some moves -> withMoves moves
