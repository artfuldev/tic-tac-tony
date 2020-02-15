namespace TicTacTony.Core

open Positions
open Cell
open System

type Board =
  private 
  | Board of Cell array array

module Board =
  let private cells =
    Array.map (Array.map (fun _ -> Empty)) grid

  let empty =
    Board cells

  let forMoves moves =
    Board (Array.map (Array.map (create moves)) grid)

  let print = function
    | Board grid -> String.Join ("\n", Array.map (fun row -> String.Join (" ", Array.map toString row)) grid)
