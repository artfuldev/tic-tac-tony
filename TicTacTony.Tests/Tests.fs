namespace TicTacTony.Tests

open FsUnit
open Xunit

open TicTacTony.Core
open Matchers
open Game

module Tests =
    
    let private fail _ = failwith "impossible"

    let private move (game: IGame) position =
        move position (game :?> IPlayable) |> Option.defaultWith fail :> IGame

    let private position value =
        [NW; N; NE; W; C; E; SW; S; SE] |> Seq.find (string >> ((=) value))

    let private parse value =
        let xs (value: string) = value.Split " " |> Seq.filter ((<>) "")
        in xs value |> Seq.map position |> Seq.fold move (NewGame :> IGame)

    [<Theory>]
    [<InlineData ("NW N", "NE", "NW N NE")>]
    [<InlineData ("", "NW", "NW")>]
    let ``In a playable game, a valid move can be made to get a resulting game``
        (moves: string) (x: string) (result: string) =
        let expected = result |> parse
        in x |> position |> move (moves |> parse) |> should match' expected
        
    [<Theory>]
    [<InlineData ("NW N NE", "NW N")>]
    [<InlineData ("NW N SE E C", "NW N SE E")>]
    [<InlineData ("NW C SE N S SW NE E W", "NW C SE N S SW NE E")>]
    let ``Once a move is played, it can be taken back``
        (moves: string) (previous: string) =
        match moves |> parse with
        | :? IUndoable as u -> u |> undo |> should match' (previous |> parse)
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    [<InlineData ("NW C SE N S SW NE E W", "_")>]
    let ``In a completed game, the winner can be queried``
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
        moves |> parse |> playerAt (position x) |> should match' player

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", false)>]
    [<InlineData ("NW C SE N S SW NE E W", true)>]
    let ``In a filled game, is draw can be queried``
        (moves: string) (drawn: bool) =
        match moves |> parse with
        | :? IFull as f -> f |> isDraw |> should equal drawn
        | _ -> fail ()
