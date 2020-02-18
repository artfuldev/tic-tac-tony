namespace TicTacTony.Core

open Move

type Moves = private | First of Move | Next of Move * Moves

module Moves =

  let rec internal playerAt x = function
    | First (Move (y, p)) when x = y -> Some p
    | Next (Move (y, p), _) when x = y -> Some p
    | Next (_, ms) -> playerAt x ms
    | _ -> None

  let rec internal count = function | First _ -> 1 | Next (_, ms) -> 1 + count ms

  let rec private list = function | First m -> [m] | Next (m, ms) -> m::(list ms) |> List.rev

  let internal positions = list >> Seq.map position

  let internal undo = function | First _ -> None | Next (_, ms) -> Some ms

  let internal make m = function | None -> First m | Some ms -> Next (m, ms)
