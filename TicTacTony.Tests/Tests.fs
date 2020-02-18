namespace TicTacTony.Tests

open FsUnit
open FsCheck
open Xunit

open TicTacTony.Core

module Tests =

    let fail _ =
        false |> should equal true

    let board game =
        match game with
        | Fresh (g, _) | Played (g, _, _)  | Won (g, _, _, _)
        | Drawn (g, _, _, _) -> g.Board

    let assertEquals expected actual =
        board actual |> should equal (board expected)

    let move game x =
        match game with
        | Fresh (_, p) | Played (_, _, p) ->
           p.Moves
           |> Seq.find (Move.position >> ((=) x))
           |> p.Move
        | _ -> failwith "not found"

    let play game positions =
        positions
        |> Seq.map Positions.parse
        |> Seq.choose id
        |> Seq.fold move game

    [<Fact>]
    let ``New game is a fresh game where the board is empty`` () =
        match Game.NewGame with
        | Fresh (g, _) ->
            g.Positions
            |> Seq.map g.PlayerAt
            |> Seq.iter (should equal None)
        | _ -> fail ()

    [<Fact>]
    let ``New game is a fresh game where the player is X`` () =
        match Game.NewGame with
        | Fresh (_, p) -> p.Player |> Player.toString |> should equal "X"
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N", "NE", "NW N NE")>]
    [<InlineData ("", "NW", "NW")>]
    let ``In a playable game, a valid move can be made to get a resulting game`` (moves: string)  (next: string) (result: string) =
        let current = moves.Split " " |> play Game.NewGame
        let expected = result.Split " " |> play Game.NewGame
        let result = move current (Option.get (Positions.parse next))
        in assertEquals expected result
        
    [<Theory>]
    [<InlineData ("NW N NE", "NW N")>]
    let ``Once a move is played, it can be undone to get back to the previous game state`` (moves: string)  (previous: string) =
        let current = moves.Split " " |> play Game.NewGame
        let expected = previous.Split " " |> play Game.NewGame
        in
            match current with
            | Played (_, u, _) | Won (_, _, _, u) | Drawn (_, _, _, u) ->
                assertEquals expected (u.TakeBack ())
            | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    [<InlineData ("NW C SE N S SW NE E W", "_")>]
    let ``In a completed game, the winner can be queried`` (moves: string)  (winner: string) =
        match moves.Split " " |> play Game.NewGame with
        | Won (_, _, o, _) | Drawn (_, _, o, _) ->
            o.WhoWon ()
            |> Option.map Player.toString
            |> Option.defaultValue "_"
            |> should equal winner
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", "NW", "X")>]
    [<InlineData ("NW C SE N S SW NE E W", "E", "O")>]
    [<InlineData ("NW C", "SE", "_")>]
    [<InlineData ("", "SE", "_")>]
    let ``In any game, player at can be queried`` (moves: string)  (position: string) (player: string) =
        match moves.Split " " |> play Game.NewGame with
        | Fresh (g, _) | Played (g, _, _) | Won (g, _, _, _)
        | Drawn (g, _, _ , _) ->
            g.Positions
            |> Seq.find (Positions.toString >> ((=) position))
            |> g.PlayerAt
            |> Option.map Player.toString
            |> Option.defaultValue "_"
            |> should equal player

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", false)>]
    [<InlineData ("NW C SE N S SW NE E W", true)>]
    let ``In a filled game, is draw can be queried`` (moves: string)  (drawn: bool) =
        match moves.Split " " |> play Game.NewGame with
        | Won (_, Some f, _, _) | Drawn (_, f, _, _) ->
            f.IsDraw () |> should equal drawn
        | _ -> fail ()
