namespace TicTacTony.Core

open Board
open Helpers


type IGame = inherit IBoard abstract member PlayerAt: Position -> Player option
type IHavePlayer = abstract member Player: Player
and IUndoable = inherit IGame abstract member Undo: unit -> IPlayable
and Move = internal Move of Position * (unit -> IUndoable)
and IHaveMoves = abstract member Moves: Move seq
and IMove = inherit IHaveMoves abstract member Move: Move -> IUndoable
and IPlayable = inherit IGame inherit IMove inherit IHavePlayer
type internal IPlaying = inherit IPlayable inherit IUndoable
type IOver = inherit IUndoable abstract member WhoWon: unit -> Player option
type IFull = inherit IOver abstract member IsDraw: unit -> bool
type internal IWon = inherit IUndoable inherit IOver
type internal IWinnable = inherit IPlaying

type internal Won (x: Position, p: IWinnable) =
    interface IWon with
        member g.PlayerAt x = playerAt x g
        member _.ToMap () = p |> play p.Player x
        member _.Undo () = p :> IPlayable
        member _.WhoWon () = p.Player |> Some

type internal Drawn (position: Position, previous: IPlayable) =
    interface IOver with
        member g.PlayerAt x = playerAt x g
        member _.ToMap () = previous |> play previous.Player position
        member _.Undo () = previous
        member _.WhoWon () = None
    interface IFull with member _.IsDraw () = true

type internal WonOnLastMove (x: Position, p: IWinnable) =
    inherit Won (x, p)
    interface IFull with member _.IsDraw () = false

type internal Playing  (x: Position, p: IPlayable) =
    abstract member _Move: Position -> (unit -> IUndoable)
    interface IPlaying with
        member g.PlayerAt x = playerAt x g
        member _.Player = if p.Player = X then O else X
        member _.ToMap () = p |> play p.Player x
        member _.Undo () = p
        member _.Move (Move (_, f)) = f ()
        member g.Moves =
            let move x = Move (x, g._Move x)
            in positions |> Seq.where (free g) |> Seq.map move
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
        member g.PlayerAt x = playerAt x g
        member _.ToMap () = Map.empty
        member _.Player = X
        member _.Move (Move (_, f)) = f ()
        member g.Moves =
            let move x = Move (x, fun () -> Playing (x, g) :> IUndoable)
            in positions |> Seq.map move

module Game =
    
    let NewGame = New () :> IPlayable

    let toString = toString

    let positions = positions

    let position (Move (x, _)) = x
