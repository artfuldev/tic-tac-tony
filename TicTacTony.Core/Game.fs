namespace TicTacTony.Core


type IFull = private | Draw | NoDraw

type IOver = private | ByWin of Player | ByDraw

type IGame = private | New | Played of Position * IGame

and IUndoable = private | Previous of IGame

type IPlayable = private | Playable of Player * Move list

and Move = private | Move of Position * (unit -> Game)

and Game =
    | Game of
        IGame *
        IPlayable option * IUndoable option * IOver option * IFull option

module Game =

    open Board
    open Helpers
    
    let other = function | X -> O | O -> X

    let rec private xOrO = function
        | New -> X | Played (_, g) -> g |> xOrO |> other

    let rec private board = function
        | New -> Map.empty | Played (x, g) -> make x (xOrO g) (board g)

    let rec private create game =
        let p = game |> xOrO
        let full =
            let full = if game |> board |> isWon then NoDraw else Draw
            in if game |> board |> Map.count |> (=) 9 then Some full else None
        let over =
            let b = game |> board
            let over = match b |> winner with | Some x -> ByWin x | _ -> ByDraw
            in if b |> isWon || full <> None then Some over else None
        let playable =
            let moves game =
                let move pos = Move (pos, Played (pos, game) |> create |> k)
                in game |> board |> unoccupied |> List.map move
            if over <> None then None else Playable (p, game |> moves) |> Some
        let undoable =
            match game with | New -> None | Played (_, g) -> Previous g |> Some
        in Game (game, playable, undoable, over, full)

    let NewGame = create New

    let moves (Playable (_, moves)) = moves

    let make = function Move (_, f) -> f ()

    let takeBack (Previous previous) = create previous

    let whoWon = function | ByWin x -> Some x | ByDraw -> None

    let isDraw = function | Draw -> true | NoDraw -> false

    let playerAt = function Game (g, _, _, _, _) -> g |> board |> flip playerAt

    let player (Playable (player, _)) = player

    let toString = function Game (g, _, _, _, _) -> g |> board |> toString

    let positions = positions
    
    let position = function Move (x, _) -> x
