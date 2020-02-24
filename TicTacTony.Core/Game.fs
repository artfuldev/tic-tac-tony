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

type internal Won (x: Position, p: IWinnable) =
    interface IWon with
        member _.ToMap () = p |> play p.Player x
        member _.Undo () = p :> IPlayable
        member _.WhoWon () = p.Player |> Some

type internal Drawn (position: Position, previous: IPlayable) =
    interface IOver with
        member _.ToMap () = previous |> play previous.Player position
        member _.Undo () = previous
        member _.WhoWon () = None
    interface IFull with member _.IsDrawn () = true

type internal WonOnLastMove (x: Position, p: IWinnable) =
    inherit Won (x, p)
    interface IFull with member _.IsDrawn () = false

type internal Playing  (x: Position, p: IPlayable) =
    abstract member _Move: Position -> (unit -> IUndoable)
    interface IPlaying with
        member _.Player = if p.Player = X then O else X
        member _.ToMap () = p |> play p.Player x
        member _.Undo () = p
        member g.Play =
            let isEmpty = flip unoccupied g
            let collect map x = x |> g._Move |> flip (Map.add x) map
            in positions |> Seq.where isEmpty |> Seq.fold collect Map.empty    
    default g._Move x =
        fun () ->
            let isEmpty = flip unoccupied g
            let played = positions |> Seq.where (not << isEmpty) |> Seq.length
            let winnable = played >= 3
            in 
                if winnable
                then Winnable(x, g) :> IUndoable
                else Playing (x, g) :> IUndoable

and internal Winnable (x: Position, p: IPlayable) =
    inherit Playing (x, p)
    interface IWinnable
    override g._Move x =
        fun () -> 
            let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
            let isEmpty = flip unoccupied g
            let played = positions |> Seq.where (not << isEmpty) |> Seq.length
            let last = played = 8
            let win = isWin (g :> IHavePlayer).Player x g
            in
                if last then
                    if win
                    then WonOnLastMove (x, g) :> IUndoable
                    else Drawn (x, g) :> IUndoable
                else
                    if win
                    then Won (x, g) :> IUndoable
                    else Winnable (x, g) :> IUndoable

type internal New () =
    interface IPlayable with
        member _.ToMap () = Map.empty
        member _.Player = X
        member p.Play =
            let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
            let isEmpty = flip unoccupied p
            let collect map x =
                Playing (x, p) :> IUndoable |> k |> flip (Map.add x) map
            in positions |> Seq.where isEmpty |> Seq.fold collect Map.empty

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

    let positions = positions
