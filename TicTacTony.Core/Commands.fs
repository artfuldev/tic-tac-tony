namespace TicTacTony.Core

type MakeMove =
  unit -> MoveResult

and MoveToMake =
  internal {
    name: string
    make: MakeMove
  }

and MoveResult =
  | XToMove of Board * Command list
  | OToMove of Board * Command list
  | GameWon of Board * Command list
  | GameDrawn of Board * Command list

and Command =
  private
  | New
  | Play of Move * Board
  | TakeBack of Moves

module Commands =
  let available = [ New; ]

