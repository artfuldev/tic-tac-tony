namespace TicTacTony.Core

open Option
open System

type private Board = Map<Position, Player option>
type Unplayed = private | Unplayed
type PlayedMove1Of9 = private PlayedMove1Of9 of Position
type PlayedMove2Of9 = private PlayedMove2Of9 of Position * PlayedMove1Of9
type PlayedMove3Of9 = private PlayedMove3Of9 of Position * PlayedMove2Of9
type PlayedMove4Of9 = private PlayedMove4Of9 of Position * PlayedMove3Of9
type PlayedMove5Of9 = private PlayedMove5Of9 of Position * PlayedMove4Of9
type WonOnMove5 = private WonOnMove5 of Position * PlayedMove4Of9
type PlayedMove6Of9 = private PlayedMove6Of9 of Position * PlayedMove5Of9
type WonOnMove6 = private WonOnMove6 of Position * PlayedMove5Of9
type PlayedMove7Of9 = private PlayedMove7Of9 of Position * PlayedMove6Of9
type WonOnMove7 = private WonOnMove7 of Position * PlayedMove6Of9
type PlayedMove8Of9 = private PlayedMove8Of9 of Position * PlayedMove7Of9
type WonOnMove8 = private WonOnMove8 of Position * PlayedMove7Of9
type PlayedMove9Of9 = private PlayedMove9Of9 of Position * PlayedMove8Of9
type WonOnMove9 = private WonOnMove9 of Position * PlayedMove8Of9
type Played =
    | Played1 of PlayedMove1Of9
    | Played2 of PlayedMove2Of9
    | Played3 of PlayedMove3Of9
    | Played4 of PlayedMove4Of9
    | Played5 of PlayedMove5Of9
    | Played6 of PlayedMove6Of9
    | Played7 of PlayedMove7Of9
    | Played8 of PlayedMove8Of9

type Won =
    | After5 of WonOnMove5
    | After6 of WonOnMove6
    | After7 of WonOnMove7
    | After8 of WonOnMove8
    | After9 of WonOnMove9
type Game =
    | Fresh of Unplayed
    | Played of Played
    | Won of Won
    | Drawn of PlayedMove9Of9

    static member NewGame () = Fresh Unplayed

    static member private _Board (Unplayed) : Board =
        Map.empty
    static member private _Board (PlayedMove1Of9 x) : Board =
        Map.add x (Some X) (Game._Board Unplayed)
    static member private _Board (PlayedMove2Of9 (x, previous)) : Board =
        Map.add x (Some O) (Game._Board previous)
    static member private _Board (PlayedMove3Of9 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)
    static member private _Board (PlayedMove4Of9 (x, previous)) : Board =
        Map.add x (Some O) (Game._Board previous)
    static member private _Board (PlayedMove5Of9 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)
    static member private _Board (WonOnMove5 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)
    static member private _Board (PlayedMove6Of9 (x, previous)) : Board =
        Map.add x (Some O) (Game._Board previous)
    static member private _Board (WonOnMove6 (x, previous)) : Board =
        Map.add x (Some O) (Game._Board previous)
    static member private _Board (PlayedMove7Of9 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)
    static member private _Board (WonOnMove7 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)
    static member private _Board (PlayedMove8Of9 (x, previous)) : Board =
        Map.add x (Some O) (Game._Board previous)
    static member private _Board (WonOnMove8 (x, previous)) : Board =
        Map.add x (Some O) (Game._Board previous)
    static member private _Board (PlayedMove9Of9 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)
    static member private _Board (WonOnMove9 (x, previous)) : Board =
        Map.add x (Some X) (Game._Board previous)

    static member private _PlayerAt (position: Position, board: Board) =
        board |> Map.tryFind position |> Option.defaultValue None
    static member PlayerAt (position: Position, game: Unplayed) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove1Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove2Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove3Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove4Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove5Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: WonOnMove5) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove6Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: WonOnMove6) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove7Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: WonOnMove7) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove8Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: WonOnMove8) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: PlayedMove9Of9) =
        Game._PlayerAt (position, Game._Board (game))
    static member PlayerAt (position: Position, game: WonOnMove9) =
        Game._PlayerAt (position, Game._Board (game))

    static member private _Winner (board: Board) =
        let winner = function
            | [ Some a; Some b; Some c ] ->
                if (a = b && b = c) then Some a else None
            | _ -> None
        in
            [ [ NW;  N; NE ]; [  W;  C;  E ]; [ SW;  S; SE ]
            ; [ NW;  W; SW ]; [  N;  C;  S ]; [ NE;  E; SE ]
            ; [ NW;  C; SE ]; [ NE;  C; SW ]
            ]
            |> Seq.map (List.map (fun x -> Game._PlayerAt (x, board)) >> winner)
            |> Seq.choose id
            |> Seq.tryHead

    static member private _IsWon =
        Game._Winner >> Option.isSome

    static member private _Occupied position (board: Board) =
        Game._PlayerAt (position, board) <> None

    static member Move (position: Position, Unplayed) =
        PlayedMove1Of9 position
    static member Move (position: Position, game: PlayedMove1Of9) =
        if game |> Game._Board |> Game._Occupied position
        then Error "Position already occupied"
        else (position, game) |> PlayedMove2Of9 |> Ok
    static member Move (position: Position, game: PlayedMove2Of9) =
        if game |> Game._Board |> Game._Occupied position
        then Error "Position already occupied"
        else (position, game) |> PlayedMove3Of9 |> Ok
    static member Move (position: Position, game: PlayedMove3Of9) =
        if game |> Game._Board |> Game._Occupied position
        then Error "Position already occupied"
        else (position, game) |> PlayedMove4Of9 |> Ok
    static member Move (position: Position, game: PlayedMove4Of9) =
        if game |> Game._Board |> Game._Occupied position then
            Error "Position already occupied"
        else
            let move = (position, game)
            in
                if move |> PlayedMove5Of9 |> Game._Board |> Game._IsWon
                then move |> WonOnMove5 |> Choice2Of2 |> Ok
                else move |> PlayedMove5Of9 |> Choice1Of2 |> Ok
    static member Move (position: Position, game: PlayedMove5Of9) =
        if game |> Game._Board |> Game._Occupied position then
            Error "Position already occupied"
        else
            let move = (position, game)
            in
                if move |> PlayedMove6Of9 |> Game._Board |> Game._IsWon
                then move |> WonOnMove6 |> Choice2Of2 |> Ok
                else move |> PlayedMove6Of9 |> Choice1Of2 |> Ok
    static member Move (position: Position, game: PlayedMove6Of9) =
        if game |> Game._Board |> Game._Occupied position then
            Error "Position already occupied"
        else
            let move = (position, game)
            in
                if move |> PlayedMove7Of9 |> Game._Board |> Game._IsWon
                then move |> WonOnMove7 |> Choice2Of2 |> Ok
                else move |> PlayedMove7Of9 |> Choice1Of2 |> Ok
    static member Move (position: Position, game: PlayedMove7Of9) =
        if game |> Game._Board |> Game._Occupied position then
            Error "Position already occupied"
        else
            let move = (position, game)
            in
                if move |> PlayedMove8Of9 |> Game._Board |> Game._IsWon
                then move |> WonOnMove8 |> Choice2Of2 |> Ok
                else move |> PlayedMove8Of9 |> Choice1Of2 |> Ok
    static member Move (position: Position, game: PlayedMove8Of9) =
        if game |> Game._Board |> Game._Occupied position then
            Error "Position already occupied"
        else
            let move = (position, game)
            in
                if move |> PlayedMove9Of9 |> Game._Board |> Game._IsWon
                then move |> WonOnMove9 |> Choice2Of2 |> Ok
                else move |> PlayedMove9Of9 |> Choice1Of2 |> Ok

    static member TakeBack (PlayedMove1Of9 _) = Unplayed
    static member TakeBack (PlayedMove2Of9 (_, previous)) = previous
    static member TakeBack (PlayedMove3Of9 (_, previous)) = previous
    static member TakeBack (PlayedMove4Of9 (_, previous)) = previous
    static member TakeBack (PlayedMove5Of9 (_, previous)) = previous
    static member TakeBack (WonOnMove5 (_, previous)) = previous
    static member TakeBack (PlayedMove6Of9 (_, previous)) = previous
    static member TakeBack (WonOnMove6 (_, previous)) = previous
    static member TakeBack (PlayedMove7Of9 (_, previous)) = previous
    static member TakeBack (WonOnMove7 (_, previous)) = previous
    static member TakeBack (PlayedMove8Of9 (_, previous)) = previous
    static member TakeBack (WonOnMove8 (_, previous)) = previous
    static member TakeBack (PlayedMove9Of9 (_, previous)) = previous
    static member TakeBack (WonOnMove9 (_, previous)) = previous

    static member WhoWon (_: WonOnMove5) = Some X
    static member WhoWon (_: WonOnMove6) = Some O
    static member WhoWon (_: WonOnMove7) = Some X
    static member WhoWon (_: WonOnMove8) = Some O
    static member WhoWon (_: WonOnMove9) = Some X
    static member WhoWon (_: PlayedMove9Of9) = None

    static member IsDraw (_: WonOnMove9) = false
    static member IsDraw (_: PlayedMove9Of9) = true

    static member private _ToString (board: Board) =
        let playerAt = fun x -> Game._PlayerAt (x, board)
        let positions = [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]
        let rows = positions |> Seq.chunkBySize 3 |> Seq.map (Seq.map playerAt)
        let row r = String.Join(" ", Seq.map (map string >> defaultValue "_") r)
        in String.Join ("\n", Seq.map row rows)

    static member ToString (game: Unplayed) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove1Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove2Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove3Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove4Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove5Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: WonOnMove5) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove6Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: WonOnMove6) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove7Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: WonOnMove7) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove8Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: WonOnMove8) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: PlayedMove9Of9) =
        game |> Game._Board |> Game._ToString
    static member ToString (game: WonOnMove9) =
        game |> Game._Board |> Game._ToString
