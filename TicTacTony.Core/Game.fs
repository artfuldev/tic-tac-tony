namespace TicTacTony.Core

open Moves
open Move
open Helpers
open Positions
open Board

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

  let newGame =
    Playable New

  let private _show board player result =
    let player' = match player with | Some p -> sprintf "\nTo play: %s" (Player.toString p) | None -> ""
    let result' =
      match result with
      | Some (Won winner) -> sprintf "\nWon by %s" (Player.toString winner)
      | Some (Drawn) -> "\nDrawn"
      | None -> ""
    sprintf "%s%s%s" (print board) player' result'

  let private _board = function
    | Playable New -> empty
    | Playable (InProgress moves) | Complete (moves, _) -> forMoves moves
    
  let player = function
    | New -> X
    | InProgress moves -> if count moves % 2 <> 1 then X else O

  let private _player = function
    | Playable playable -> Some (player playable)
    | Complete _ -> None

  let private _result = function
    | Playable _ -> None
    | Complete (_, result) -> Some result

  let show game =
    _show (_board game) (_player game) (_result game)

  let fold ifPlayable ifComplete = function
    | Playable playable -> ifPlayable playable
    | Complete (moves, result) -> ifComplete (moves, result)

  let playerAt position = function
    | Playable New -> None
    | Playable (InProgress moves) -> playerAt position moves
    | Complete (moves, _) -> playerAt position moves

  let available = function
    | New -> all
    | InProgress moves -> all |> List.filter (not << ((flip List.contains) (positions moves)))

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
