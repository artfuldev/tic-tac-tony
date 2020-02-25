namespace TicTacTony.Core


type IGame = { Board: Board; PlayerAt: Position -> Player option }

type IFull = { IsDraw: unit -> bool }

type IOver = { WhoWon: unit -> Player option }
  
type IUndoable = { TakeBack: unit -> Game }

and IPlayable = { Player: Player; Move: Move -> Game; Moves: Move seq }

and Game =
    | Fresh of IGame * IPlayable
    | Played of IGame * IUndoable * IPlayable
    | Won of IGame * IFull option * IOver * IUndoable
    | Drawn of IGame * IFull * IOver * IUndoable

module Game =

    open Board
    open Helpers
    
    let private game board =
        { Board = board; PlayerAt = flip playerAt board; }

    let private full board =
        let isDraw _ = s (isFull >> (&&)) (not << isWon) board
        in { IsDraw = isDraw }

    let private over board =
        let whoWon _ = winner board
        in { WhoWon = whoWon }

    let private move playable undoable board move =
        let board = make move board
        let game = game board
        let full = full board
        let undoable = undoable board
        let over = over board
        let playable = playable board
        let isFull = isFull board
        let full' = if isFull then Some full else None
        let won = Won (game, full', over, undoable)
        let drawn = Drawn (game, full, over, undoable)
        let played = Played (game, undoable, playable)
        in if isWon board then won else if isFull then drawn else played

    let rec private undoable board =
        let takeBack _ =
            match undo board with
            | Board [] -> NewGame
            | board -> Played (game board, undoable board, playable board)
        in { TakeBack = takeBack }
  
    and private playable board =
        let player = player board
        let moves = unoccupied board |> Seq.map (fun x -> Move(x, player))
        in
            { Player = player
            ; Moves = moves
            ; Move = move playable undoable board
            }

    and NewGame = Fresh (game (Board []), playable (Board []))
