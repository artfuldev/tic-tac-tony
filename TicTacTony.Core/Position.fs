namespace TicTacTony.Core

type Position =
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

  let all =
    [ NW; N; NE;
      W; C; E;
      SW; S; SE ]

  let wins = [|
    [| NW; N; NE |]
    [| W; C; E |]
    [| SW; S; SE |]
    [| NW; W; SW |]
    [| N; C; S |]
    [| NE; E; SE |]
    [| NW; C; SE |]
    [| NE; C; SW |]
  |]
