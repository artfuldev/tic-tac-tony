namespace TicTacTony.Core

open Moves
open Move
open Helpers
open Positions

type PlayableGame =
  private
  | New
  | InProgress of Moves
type Game =
  private
  | Playable of PlayableGame
  | Complete of Moves * GameResult

module Game =
  let private isWin player' moves =
    Array.exists (Array.forall (flip List.contains (positionsOf player' moves))) wins

  let player = function
    | New -> X
    | InProgress moves -> if count moves % 2 <> 1 then X else O

  let playerAt position = function
    | Playable New -> None
    | Playable (InProgress moves) -> playerAt position moves
    | Complete (moves, _) -> playerAt position moves

  let available = function
    | New -> all
    | InProgress moves -> all |> List.filter (flip List.contains (positions moves))

  let move position' = function
    | New -> InProgress (First (Move (position', player New))) |> Playable |> Ok
    | InProgress moves ->
      match Moves.playerAt position' moves with
      | Some _ -> Error "position already taken"
      | None ->
        let player' = player (InProgress moves)
        let moves' = Next (Move (position', player'), moves)
        let result =
          if isWin player' moves'
          then Complete (moves', Won player')
          else if count moves' = 9 then Complete (moves', Drawn) else Playable (InProgress moves')
        in Ok result
  
  let winner = function
    | Won player -> Some player
    | Drawn -> None
        
  let takeBack = function
    | Playable New -> None
    | Playable (InProgress (First _)) -> Playable New |> Some
    | Playable (InProgress (Next (_, moves))) -> Playable (InProgress moves) |> Some
    | Complete (moves, _) -> Playable (InProgress moves) |> Some
