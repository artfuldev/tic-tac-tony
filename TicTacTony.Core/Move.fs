namespace TicTacTony.Core

type Move =
  private 
  | Move of Position * Player

module Move =
  let position = function
  | Move (position, _) -> position
  let player = function
  | Move (_, player) -> player
