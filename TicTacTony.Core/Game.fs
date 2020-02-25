namespace TicTacTony.Core


type IFull = private | Draw | NoDraw

type IOver = private | ByWin of Player | ByDraw

and IGame = private | New | Played of Position * IGame

and IUndoable = private | Previous of IGame

and IPlayable = private | Moves of Move list

and Move = private | Move of Position * (unit -> Game)

and Game =
    | Game of
        IGame *
        IPlayable option * IUndoable option * IOver option * IFull option

module Game =

    open Board
    open Helpers

    let private other = function | X -> O | O -> X

    let rec private player = function
        | New -> X | Played (_, g) -> g |> player |> other

    let rec private board = function
        | New -> Map.empty | Played (x, g) -> Board.make x (player g) (board g)

    let private full game =
        let board = game |> board
        in
            if (not << isFull) board
            then None
            else if (not << isWon) board then Some Draw else Some NoDraw
    
    let private over game =
        let board = game |> board
        let over =
            match board |> winner with | Some x -> ByWin x | _ -> ByDraw
        in if not <| s (isFull >> (||)) isWon board then None else Some over                
    
    let private undoable = function
        | New -> None | Played (_, g) -> Previous g |> Some

    let rec private playable game =
        if game |> over |> Option.isSome
        then None
        else Moves (game |> _moves) |> Some

    and private _moves game =
        let move pos = Move (pos, fun _ -> move pos game)
        in game |> board |> unoccupied |> List.map move

    and private create game =
        let undoable = undoable game
        let full = full game
        let over = over game
        let playable = playable game
        in Game (game, playable, undoable, over, full)
    
    and private move position game = Played (position, game) |> create        

    and NewGame = create New

    let moves (Moves moves) = moves

    let make = function Move (_, f) -> f ()

    let takeBack (Previous previous) = create previous

    let whoWon = function | ByWin x -> Some x | ByDraw -> None

    let isDraw = function | Draw -> true | NoDraw -> false

    let playerAt = function Game (g, _, _, _, _) -> g |> board |> flip playerAt

    let toString = function Game (g, _, _, _, _) -> g |> board |> toString

    let positions = positions
    
    let position = function Move (x, _) -> x
