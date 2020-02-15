namespace TicTacTony.Core

type Player = 
  | X
  | O

module Player =
  let toString = function
    | X -> "X"
    | O -> "O"
