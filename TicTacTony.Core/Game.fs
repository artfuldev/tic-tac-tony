namespace TicTacTony.Core

open Helpers

type IGame =
  { Board: Board
  ; PlayerAt: Position -> Player option
  }
  
type IFilled =
  { IsDraw: unit -> bool
  }
  
type ICompleted =
  { WhoWon: unit -> Player option
  }
  
type IUndoable =
  { TakeBack: unit -> Game
  }

and IPlayable =
  { Player: Player
  ; Move: Position -> Game
  ; Choices: Position list
  }

and NewGame =
  { Game: IGame
  ; Playable: IPlayable
  }

and InProgressGame =
  { Game: IGame
  ; Undoable: IUndoable
  ; Playable: IPlayable
  }

and WonGame =
  { Game: IGame
  ; Filled: IFilled option
  ; Completed: ICompleted
  ; Undoable: IUndoable
  }

and DrawnGame =
  { Game: IGame
  ; Filled: IFilled
  ; Completed: ICompleted
  ; Undoable: IUndoable
  }

and Game =
  | New of IGame * IPlayable
  | InProgress of IGame * IUndoable * IPlayable
  | Won of IGame * IFilled option * ICompleted * IUndoable
  | Drawn of IGame * IFilled * ICompleted * IUndoable

module Game =

  let internal game board =
    { Board = board; PlayerAt = flip Board.playerAt board }

  let internal _filled board =
    { IsDraw = if Option.isNone (Board.winner board) then constant true else constant false }
  
  let internal filled board =
    match Board.moves board with
    | Some moves -> if Moves.count moves = 9 then Some (_filled board) else None
    | _ -> None

  let internal completed winner =
    { WhoWon = constant winner }

  let internal move playable undoable player board position =
    let moves' = Moves.make (Move (position, player)) (Board.moves board)
    let board' = Played moves'
    let game = game board'
    let filled = filled board'
    let undoable = undoable moves'
    let winner = Board.winner board'
    let completed = completed winner
    let playable = playable board'
    in
      match winner with
      | Some _ -> Won (game, filled, completed, undoable)
      | None ->
        match filled with
        | Some filled -> Drawn (game, filled, completed, undoable)
        | None -> InProgress (game, undoable, playable)

  let rec internal undoable moves =
    let game' =
      match Moves.undo moves with
      | Some moves ->
        let board = Played moves
        in InProgress (game board, undoable moves, playable board)
      | None -> NewGame
    in { TakeBack = constant game' }
  
  and internal playable board =
    let player = Board.player board
    let choices = Board.unoccupied board |> List.map (Move.create player >> Move.position)
    in { Player = player; Choices = choices; Move = move playable undoable player board }

  and NewGame =
      New (game Empty, playable Empty)
    
    
