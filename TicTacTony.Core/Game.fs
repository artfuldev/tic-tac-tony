namespace TicTacTony.Core

open Board
open Player

type Full = private | Draw | NoDraw

type Over = private | ByWin of Player | ByDraw

type State = private | New | Played of Position * State

and Playable = private | Playable of Move list

and Move = private | Move of Position * (unit -> IUndoable)

and Undoable = private | Previous of IPlayable

and IGame = abstract member State: State

and IPlayable = inherit IGame abstract member Playable: Playable

and IUndoable = inherit IGame abstract member Undoable: Undoable

and IOver = inherit IUndoable abstract member Over: Over

and IFull = inherit IOver abstract member Full: Full

type internal IPlaying = inherit IPlayable inherit IUndoable

type internal IWon = inherit IOver

type internal IWinnable = inherit IPlaying

module Move =
    
    let position = function Move (x, _) -> x

module internal Internal =

    let rec player = function
        | New -> X | Played (_, g) -> g |> player |> other

    let rec board = function
        | New -> Map.empty | Played (x, g) -> make x (player g) (board g)
    
    let moves (Playable moves) = moves
    
    let takeBack (Previous previous) = previous
    
    let whoWon = function | ByWin x -> Some x | ByDraw -> None
    
    let isDraw = function | Draw -> true | NoDraw -> false

    type Game (state: State) = interface IGame with member _.State = state
     
    type PlayedGame (position: Position, previous: IPlayable) =
        inherit Game (Played (position, previous.State))
        interface IUndoable with member _.Undoable = Previous previous
     
    type WonGame (position: Position, previous: IWinnable) =
        inherit PlayedGame (position, previous)
        interface IWon with member _.Over = ByWin (player previous.State)
     
    type DrawnGame (position: Position, previous: IWinnable) =
        inherit PlayedGame (position, previous)
        interface IOver with member _.Over = ByDraw
        interface IFull with member _.Full = Draw
     
    type WonOnLastMoveGame (position: Position, previous: IWinnable) =
        inherit WonGame (position, previous)
        interface IFull with member _.Full = NoDraw
     
    type InProgressGame  (position: Position, previous: IPlayable) =
        inherit PlayedGame (position, previous)
        abstract member _Move: Position -> (unit -> IUndoable)
        interface IPlaying with
            member game.Playable =
                let move position = Move (position, position |> game._Move)
                let free = (game :> IGame).State |> board |> free
                in Position.all |> List.where free |> List.map move |> Playable
        default game._Move position = fun () ->
            let occupied = not << ((game :> IGame).State |> board |> free)
            let played = Position.all |> List.where occupied |> List.length
            in
                if played >= 3
                then upcast WinnableGame (position, game)
                else upcast InProgressGame (position, game)
     
    and WinnableGame (position: Position, previous: IPlayable) =
        inherit InProgressGame (position, previous)
        interface IWinnable
        override game._Move position = fun () ->
            let board = (game :> IGame).State |> board
            let occupied = not << free board
            let played = Position.all |> List.where occupied |> List.length
            let player = (game :> IGame).State |> player
            let board' = make position player board
            let win = isWon board'
            in
                if played = 8 then
                    if win
                    then upcast WonOnLastMoveGame (position, game)
                    else upcast DrawnGame (position, game)
                else
                    if win
                    then upcast WonGame (position, game)
                    else upcast WinnableGame (position, game)
     
    type NewGame () =
        inherit Game (New)
        interface IPlayable with
            member g.Playable =
                let move x = Move (x, fun _ -> upcast InProgressGame (x, g))
                in Position.all |> List.map move |> Playable

module Game =

    open Helpers
    open Internal

    let NewGame = NewGame () :> IGame

    let moves (game: IPlayable) = moves game.Playable
    
    let make = function Move (_, f) -> f ()

    let takeBack (game: IUndoable) = takeBack game.Undoable

    let whoWon (game: IOver) = whoWon game.Over

    let isDraw (game: IFull) = isDraw game.Full

    let playerAt (game: IGame) = game.State |> board |> flip playerAt

    let player (game: IPlayable) = player game.State

    let toString (game: IGame) = game.State |> board |> toString
