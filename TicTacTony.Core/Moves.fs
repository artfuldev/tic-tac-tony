namespace TicTacTony.Core

open Move

type Moves =
  private
  | First of Move
  | Next of Move * Moves

module Moves =

  let rec playerAt position = function
    | First move -> if Move.position move = position then Some (Move.player move) else None
    | Next (move, moves) -> if Move.position move = position then Some (Move.player move) else playerAt position moves

  let rec count = function
    | First _ -> 1
    | Next (_, moves) -> 1 + count moves

  let rec list = function
    | First move -> [move]
    | Next (move, moves) -> move::(list moves) |> List.rev

  let positions =
    list >> List.map position

  let positionsOf player' =
    list >> (List.filter (player >> ((=) player'))) >> (List.map (position))

  let internal undo = function
    | First _ -> None
    | Next (_, moves) -> Some moves

  let internal make move = function
    | None -> First move
    | Some moves -> Next (move, moves)
