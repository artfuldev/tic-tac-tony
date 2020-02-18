namespace TicTacTony.Core

type Player = internal | X | O

module Player =
  let toString = function | X -> "X" | O -> "O"
