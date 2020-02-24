namespace TicTacTony.Core

open Board
open Helpers

type IGame = inherit IBoard
type IHavePlayer = abstract member Player: Player
and IUndoable = inherit IGame abstract member Undo: unit -> IPlayable
and IMove = abstract member Move: Position -> IUndoable option
and IPlayable = inherit IGame inherit IMove inherit IHavePlayer
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
        member g.Move x =
            let f r x = x |> g._Move |> flip (Map.add x) r
            let moves = positions |> Seq.where (free g) |> Seq.fold f Map.empty
            in moves |> Map.tryFind x |> Option.map ((|>) ())
    default g._Move x = fun () ->
        let played = positions |> Seq.where (not << free g) |> Seq.length
        let u x = x :> IUndoable
        in if played >= 3 then Winnable (x, g) |> u else Playing (x, g) |> u

and internal Winnable (x: Position, p: IPlayable) =
    inherit Playing (x, p)
    interface IWinnable
    override g._Move x = fun () -> 
        let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
        let played = positions |> Seq.where (not << free g) |> Seq.length
        let win = isWin (g :> IHavePlayer).Player x g
        let u x = x :> IUndoable 
        in
            if played = 8 then
                if win then WonOnLastMove (x, g) |> u else Drawn (x, g) |> u
            else
                if win then Won (x, g) |> u else Winnable (x, g) |> u

type internal New () =
    interface IPlayable with
        member _.ToMap () = Map.empty
        member _.Player = X
        member g.Move x = Some (Playing (x, g) :> IUndoable)

module Game =
    
    let NewGame = New () :> IPlayable
    
    let playerAt = playerAt

    let move x (playable: IPlayable) =  playable.Move x

    let undo (undoable: IUndoable) = undoable.Undo ()

    let whoWon (over: IOver) = over.WhoWon ()

    let isDraw (full: IFull) = full.IsDrawn ()

    let toString = toString

    let positions = positions

    let free = free
