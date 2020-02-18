namespace TicTacTony.Core

open Helpers


type IGame =
    { Board: Board
    ; PlayerAt: Position -> Player option
    ; Positions: Position seq
    }

type IFull = { IsDraw: unit -> bool }

type IOver = { WhoWon: unit -> Player option }
  
type IUndoable = { TakeBack: unit -> Game }

and IPlayable = { Player: Player; Move: Move -> Game; Moves: Move seq }

and NewGame = { Game: IGame; Playable: IPlayable }

and InProgressGame =
    { Game: IGame
    ; Undoable: IUndoable
    ; Playable: IPlayable
    }

and WonGame =
    { Game: IGame
    ; Filled: IFull option
    ; Over: IOver
    ; Undoable: IUndoable
    }

and DrawnGame =
    { Game: IGame
    ; Filled: IFull
    ; Over: IOver
    ; Undoable: IUndoable
    }

and Game =
    | Fresh of IGame * IPlayable
    | Played of IGame * IUndoable * IPlayable
    | Won of IGame * IFull option * IOver * IUndoable
    | Drawn of IGame * IFull * IOver * IUndoable

module Game =

    open Positions
    open Moves
    open Board
    
    let private game board =
        { Board = board; PlayerAt = flip playerAt board; Positions = all }

    let private full board =
        let isDraw _ = s (isFull >> (&&)) (not << isWon) board
        in { IsDraw = isDraw }

    let private over board =
        let whoWon _ = winner board
        in { WhoWon = whoWon }

    let private move playable undoable board move =
        let moves = make move (moves board)
        let board = Has moves
        let game = game board
        let full = full board
        let undoable = undoable moves
        let over = over board
        let playable = playable board
        let isFull = isFull board
        let full' = if isFull then Some full else None
        let won = Won (game, full', over, undoable)
        let drawn = Drawn (game, full, over, undoable)
        let played = Played (game, undoable, playable)
        in if isWon board then won else if isFull then drawn else played

    let rec private undoable moves =
        let takeBack _ =
            match undo moves with
            | Some moves ->
                let board = Has moves
                in Played (game board, undoable moves, playable board)
            | None -> NewGame
        in { TakeBack = takeBack }
  
    and private playable board =
        let player = player board
        let moves = unoccupied board |> Seq.map (fun x -> Move(x, player))
        in
            { Player = player
            ; Moves = moves
            ; Move = move playable undoable board
            }

    and NewGame = Fresh (game Empty, playable Empty)
