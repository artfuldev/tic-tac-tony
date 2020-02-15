namespace TicTacTony.Core

type private Cell =
  private
  | Empty
  | Taken of Player
