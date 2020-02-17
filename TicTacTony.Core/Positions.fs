namespace TicTacTony.Core

type Position =
  internal
  | NW
  | N
  | NE
  | W
  | C
  | E
  | SW
  | S
  | SE

module Positions =

  let internal all =
    [ NW; N; NE;
      W; C; E;
      SW; S; SE ]

  let toString = function
    | NW -> "NW"
    | N -> "N"
    | NE -> "NE"
    | W -> "W"
    | C -> "C"
    | E -> "E"
    | SW -> "SW"
    | S -> "S"
    | SE -> "SE"

  let parse value =
    all |> List.tryFind (toString >> ((=) value))
