namespace TicTacTony.Core

open Board
open Helpers

type IGame = inherit IBoard
type IHavePlayer = abstract member Player: Player
and IUndoable = inherit IGame abstract member Undo: unit -> IPlayable
and IAllowPlay = abstract member Play: Map<Position, unit -> IUndoable>
and IPlayable = inherit IGame inherit IAllowPlay inherit IHavePlayer
type internal IPlaying = inherit IPlayable inherit IUndoable
type IOver = inherit IUndoable abstract member WhoWon: unit -> Player option
type IFull = inherit IOver abstract member IsDrawn: unit -> bool
type internal IWon = inherit IUndoable inherit IOver
type internal IWinnable = inherit IPlaying

[<AbstractClass>]
type internal Playable  (x: Position, p: IPlayable) =
    abstract member _Play: Position -> IUndoable
    abstract member _Moves: Map<Position, unit -> IUndoable>
    interface IPlaying with
        member _.Player = if p.Player = X then O else X
        member _.ToMap () = p |> play p.Player x
        member _.Undo () = p
        member g.Play = g._Moves
    default g._Moves =
        let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
        let isEmpty = flip unoccupied p
        let collect map x = x |> g._Play |> k |> flip (Map.add x) map
        in positions |> Seq.where isEmpty |> Seq.fold collect Map.empty

[<AbstractClass>]
type internal Winnable  (x: Position, p: IPlayable) =
    inherit Playable (x, p)
    abstract member _Win: Position -> IWon
    interface IWinnable
    override g._Moves =
        let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
        let isEmpty = flip unoccupied p
        let result x =
            fun () ->
                if isWin (g :> IHavePlayer).Player x p
                then g._Win x :> IUndoable
                else g._Play x
        let collect map x = x |> result |> flip (Map.add x) map
        in positions |> Seq.where isEmpty |> Seq.fold collect Map.empty

[<AbstractClass>]
type internal Won (x: Position, p: IWinnable) =
    member _.Player = if p.Player = X then O else X
    interface IWon with
        member _.ToMap () = p |> play p.Player x
        member _.Undo () = p :> IPlayable
        member _.WhoWon () = p.Player |> Some

type internal New () =
    interface IPlayable with
        member _.ToMap () = Map.empty
        member _.Player = X
        member p.Play =
            let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
            let isEmpty = flip unoccupied p
            let collect map x =
                PlayedMove1Of9 (x, p) :> IUndoable |> k |> flip (Map.add x) map
            in positions |> Seq.where isEmpty |> Seq.fold collect Map.empty

and internal PlayedMove1Of9 (position: Position, previous: New) =
    inherit Playable (position, previous)
    override game._Play position = PlayedMove2Of9 (position, game) :> IUndoable

and internal PlayedMove2Of9 (position: Position, previous: PlayedMove1Of9) =
    inherit Playable (position, previous) with
    override game._Play position = PlayedMove3Of9 (position, game) :> IUndoable

and internal PlayedMove3Of9 (position: Position, previous: PlayedMove2Of9) =
    inherit Playable (position, previous) with
    override game._Play position = PlayedMove4Of9 (position, game) :> IUndoable

and internal PlayedMove4Of9 (position: Position, previous: PlayedMove3Of9) =
    inherit Winnable (position, previous) with
    override game._Play position = PlayedMove5Of9 (position, game) :> IUndoable
    override game._Win position = WonOnMove5 (position, game) :>  IWon

and internal PlayedMove5Of9 (position: Position, previous: PlayedMove4Of9) =
    inherit Winnable (position, previous) with
    override game._Play position = PlayedMove6Of9 (position, game) :> IUndoable
    override game._Win position = WonOnMove6 (position, game) :> IWon

and internal WonOnMove5 (position: Position, previous: PlayedMove4Of9) =
    inherit Won (position, previous)

and internal PlayedMove6Of9 (position: Position, previous: PlayedMove5Of9) =
    inherit Winnable (position, previous) with
    override game._Play position = PlayedMove7Of9 (position, game) :> IUndoable
    override game._Win position = WonOnMove7 (position, game) :> IWon

and internal WonOnMove6 (position: Position, previous: PlayedMove5Of9) =
    inherit Won (position, previous)

and internal PlayedMove7Of9 (position: Position, previous: PlayedMove6Of9) =
    inherit Winnable (position, previous) with
    override game._Play position = PlayedMove8Of9 (position, game) :> IUndoable
    override game._Win position = WonOnMove8 (position, game) :> IWon

and internal WonOnMove7 (position: Position, previous: PlayedMove6Of9) =
    inherit Won (position, previous)

and internal PlayedMove8Of9 (position: Position, previous: PlayedMove7Of9) =
    inherit Winnable (position, previous) with
    override game._Play position = Drew (position, game) :> IUndoable
    override game._Win position = WonOnLastMove (position, game) :> IWon

and internal WonOnMove8 (position: Position, previous: PlayedMove7Of9) =
    inherit Won (position, previous)

and internal Drew (position: Position, previous: PlayedMove8Of9) =
    member _.Player = (previous :> IPlayable).Player
    interface IOver with
        member g.ToMap () = previous |> play g.Player position
        member _.Undo () = previous :> IPlayable
        member _.WhoWon () = None
    interface IFull with member _.IsDrawn () = true

and internal WonOnLastMove (x: Position, p: PlayedMove8Of9) =
    inherit Won (x, p)
    interface IFull with member _.IsDrawn () = false

module Game =
    
    let NewGame = New () :> IPlayable
    
    let playerAt = playerAt

    let move x (playable: IPlayable) = 
        playable.Play |> Map.tryFind x |> Option.map ((|>) ())

    let undo (undoable: IUndoable) =
        undoable.Undo ()

    let whoWon (over: IOver) =
        over.WhoWon ()

    let isDraw (full: IFull) =
        full.IsDrawn ()

    let toString = toString
