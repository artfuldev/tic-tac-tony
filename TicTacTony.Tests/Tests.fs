namespace TicTacTony.Tests

open FsUnit
open Xunit
open FsCheck.Xunit

open TicTacTony.Core
open Matchers
open Game
open Move
open Generators
open Position

module Tests =
    
    let private fail _ = failwith "impossible"

    let private same game game' = toString game = toString game'

    let private move (game: IGame) x =
        let f p x = p |> moves |> List.find (position >> ((=) x)) |> make
        in match game with | :? IPlayable as p -> f p x :> IGame | _ -> fail ()

    let private toPosition value = all |> Seq.find (string >> ((=) value))

    let private parse value =
        let xs (value: string) = value.Split " " |> Seq.filter ((<>) "")
        in value |> xs |> Seq.map toPosition |> Seq.fold move NewGame

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, a move played is no longer available``
        (game: IGame) =
        let same move move' = position move = position move'
        in
            match game with
            | :? IPlayable as p ->
                let test move =
                    match make move with
                    | :? IPlayable as p ->
                        p |> moves |> Seq.forall (not << same move)
                    | _ -> true = true
                in p |> moves |> Seq.forall test
            | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, the number of available moves reduces at each move``
        (game: IGame) =
        let count p = p |> moves |> List.length
        in
            match game with
            | :? IPlayable as p ->
                let test move =
                    match make move with
                    | :? IPlayable as p' -> count p >= count p'
                    | _ -> true = true
                in p |> moves |> Seq.forall test
            | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, a move returns a new value of game``
        (game: IGame) =
        match game with
        | :? IPlayable as p ->
            p |> moves |> Seq.map make |> Seq.forall (not << same game)
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Won> |])>]
    let ``In a won game, whoWon returns the previous player as winner``
        : IGame -> bool = function
        | :? IOver as o -> whoWon o = Some (o |> takeBack |> player)
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Drawn> |])>]
    let ``In a drawn game, whoWon returns none`` : IGame -> bool = function
        | :? IOver as o -> o |> whoWon = None
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Full> |])>]
    let ``In a full game, isDraw returns whether the game is drawn``
        : IGame -> bool = function
        | :? IFull as f ->
            match f |> whoWon with
            | Some _ -> f |> (not << isDraw) | _ -> f |> isDraw
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, undoing a move on the game returns the same game``
        (game: IGame) =
        match game with
        | :? IPlayable as p ->
            p |> moves |> Seq.map (make >> takeBack) |> Seq.forall (same game)
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    let ``In a won game, the winner can be queried``
        (moves: string) (winner: string) =
        match moves |> parse with
        | :? IOver as o -> o |> whoWon |> should match' winner
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", "NW", "X")>]
    [<InlineData ("NW C SE N S SW NE E W", "E", "O")>]
    [<InlineData ("NW C", "SE", "_")>]
    [<InlineData ("", "SE", "_")>]
    let ``In any game, player at can be queried``
        (moves: string) (x: string) (player: string) =
        x |> toPosition |> playerAt (moves |> parse) |> should match' player
