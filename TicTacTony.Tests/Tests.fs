namespace TicTacTony.Tests

open FsUnit
open Xunit

open TicTacTony.Core
open Matchers

module Tests =

    let private fail _ =
        false |> should equal true

    let private move game x =
        match game with
        | Fresh (_, p) | Played (_, _, p) ->
           p.Moves |> Seq.find (Move.position >> ((=) x)) |> p.Move
        | _ -> failwith "not found"

    let private play game positions =
        positions
        |> Seq.map Positions.parse
        |> Seq.choose id
        |> Seq.fold move game

    [<Theory>]
    [<InlineData ("NW N", "NE", "NW N NE")>]
    [<InlineData ("", "NW", "NW")>]
    let ``In a playable game, a valid move can be made to get a resulting game`` (moves: string)  (next: string) (result: string) =
        let current = moves.Split " " |> play Game.NewGame
        let expected = result.Split " " |> play Game.NewGame
        let result = move current (next |> Positions.parse |> Option.get)
        in result |> should beGame expected
        
    [<Theory>]
    [<InlineData ("NW N NE", "NW N")>]
    let ``Once a move is played, it can be undone to get back to the previous game state`` (moves: string)  (previous: string) =
        let current = moves.Split " " |> play Game.NewGame
        let expected = previous.Split " " |> play Game.NewGame
        in
            match current with
            | Played (_, u, _) | Won (_, _, _, u) | Drawn (_, _, _, u) ->
                u.TakeBack () |> should beGame expected
            | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    [<InlineData ("NW C SE N S SW NE E W", "_")>]
    let ``In a completed game, the winner can be queried`` (moves: string)  (winner: string) =
        match moves.Split " " |> play Game.NewGame with
        | Won (_, _, o, _) | Drawn (_, _, o, _) ->
            o.WhoWon () |> should bePlayer winner
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
            |> should bePlayer player

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", false)>]
    [<InlineData ("NW C SE N S SW NE E W", true)>]
    let ``In a filled game, is draw can be queried`` (moves: string)  (drawn: bool) =
        match moves.Split " " |> play Game.NewGame with
        | Won (_, Some f, _, _) | Drawn (_, f, _, _) ->
            f.IsDraw () |> should equal drawn
        | _ -> fail ()
