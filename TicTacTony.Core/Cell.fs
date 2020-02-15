namespace TicTacTony.Core

open Moves
open Player

type Cell =
  private
  | Empty
  | Taken of Player

module Cell =
  let create moves position =
    match playerAt position moves with
    | Some player -> Taken player
    | None -> Empty

  let toString = function
    | Taken player -> toString player
    | Empty -> "_"
