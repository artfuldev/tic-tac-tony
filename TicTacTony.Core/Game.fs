namespace TicTacTony.Core

open Board

type IGame = inherit IBoard
type IHavePlayer = abstract member Player: Player
and IUndoable = inherit IGame abstract member Previous: IPlayable
and IAllowPlay = abstract member Play: Position -> IUndoable
and IPlayable = inherit IGame inherit IAllowPlay inherit IHavePlayer
type internal IPlaying = inherit IPlayable inherit IUndoable
type IOver = inherit IUndoable abstract member Winner: Player option
type IFull = inherit IOver abstract member Drawn: bool
type internal IWon = inherit IUndoable inherit IOver
type internal IAllowWin = abstract member Win: Position -> IWon
type internal IWinnable = inherit IPlaying inherit IAllowWin

[<AbstractClass>]
type Played internal (x: Position, p: IPlayable) =
    interface IGame with member _.ToMap () = p |> play p.Player x
    interface IUndoable with member _.Previous = p
    interface IHavePlayer with member _.Player = if p.Player = X then O else X

[<AbstractClass>]
type Won internal (x: Position, p: IWinnable) =
    inherit Played(x, p)
    interface IWon with member _.Winner = p.Player |> Some

type New internal () =
    interface IPlayable with
        member _.ToMap () = Map.empty
        member _.Player = X
        member p.Play x = PlayedMove1Of9 (x, p) :> IUndoable

and PlayedMove1Of9 internal (x: Position, p: New) =
    inherit Played (x, p)
    interface IPlaying with
        member p.Play x = PlayedMove2Of9 (x, p) :> IUndoable

and PlayedMove2Of9 internal (x: Position, p: PlayedMove1Of9) =
    inherit Played (x, p)
    interface IPlaying with
        member p.Play x = PlayedMove3Of9 (x, p) :> IUndoable

and PlayedMove3Of9 internal (x: Position, p: PlayedMove2Of9) =
    inherit Played (x, p)
    interface IPlaying with
        member p.Play x = PlayedMove4Of9 (x, p) :> IUndoable

and PlayedMove4Of9 internal (x: Position, p: PlayedMove3Of9) =
    inherit Played (x, p)
    interface IWinnable with
        member p.Play x = PlayedMove5Of9 (x, p) :> IUndoable
        member p.Win x = WonOnMove5 (x, p) :> IWon

and PlayedMove5Of9 internal (x: Position, p: PlayedMove4Of9) =
    inherit Played (x, p)
    interface IWinnable with
        member p.Play x = PlayedMove6Of9 (x, p) :> IUndoable
        member p.Win x = WonOnMove6 (x, p) :> IWon

and WonOnMove5 internal (x: Position, p: PlayedMove4Of9) =
    inherit Won (x, p)

and PlayedMove6Of9 internal (x: Position, p: PlayedMove5Of9) =
    inherit Played (x, p)
    interface IWinnable with
        member p.Play x = PlayedMove7Of9 (x, p) :> IUndoable
        member p.Win x = WonOnMove7 (x, p) :> IWon

and WonOnMove6 internal (x: Position, p: PlayedMove5Of9) =
    inherit Won (x, p)

and PlayedMove7Of9 internal (x: Position, p: PlayedMove6Of9) =
    inherit Played (x, p)
    interface IWinnable with
        member p.Play x = PlayedMove8Of9 (x, p) :> IUndoable
        member p.Win x = WonOnMove8 (x, p) :> IWon

and WonOnMove7 internal (x: Position, p: PlayedMove6Of9) =
    inherit Won (x, p)

and PlayedMove8Of9 internal (x: Position, p: PlayedMove7Of9) =
    inherit Played (x, p)
    interface IWinnable with
        member p.Play x = Drew (x, p) :> IUndoable
        member p.Win x = WonOnLastMove (x, p) :> IWon

and WonOnMove8 internal (x: Position, p: PlayedMove7Of9) =
    inherit Won (x, p)

and Drew internal (x: Position, p: PlayedMove8Of9) =
    inherit Played (x, p)
    interface IOver with member _.Winner = None
    interface IFull with member _.Drawn = true

and WonOnLastMove internal (x: Position, p: PlayedMove8Of9) =
    inherit Won (x, p)
    interface IFull with member _.Drawn = false

module Game =
    
    let NewGame = New ()
    
    let playerAt = playerAt

    let move x : IPlayable -> IUndoable option = function
        | :? IWinnable as w when unoccupied x w ->
            Some (if isWin w.Player x w then w.Win x :> IUndoable else w.Play x)
        | p when unoccupied x p -> Some (p.Play x)
        | _ -> None

    let takeBack (undoable: IUndoable) =
        undoable.Previous

    let whoWon (over: IOver) =
        over.Winner

    let isDraw (full: IFull) =
        full.Drawn

    let toString = toString
