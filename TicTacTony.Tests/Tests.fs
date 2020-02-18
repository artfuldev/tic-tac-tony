namespace TicTacTony.Tests

open FsUnit
open Xunit

open TicTacTony.Core
open Board
open Matchers

module Tests =

    let private fail _ =
        false |> should equal true

    let private move game x =
        match game with
        | Fresh (_, p) | Played (_, _, p) ->
           p.Moves |> Seq.find (Move.position >> ((=) x)) |> p.Move
        | _ -> failwith "not found"
    
    let private position value = positions |> Seq.find (string >> ((=) value))

    let private parse value =
        let positions (value: string) = value.Split " " |> Seq.filter ((<>) "")
        in value |> positions |> Seq.map position |> Seq.fold move Game.NewGame

    [<Theory>]
    [<InlineData ("NW N", "NE", "NW N NE")>]
    [<InlineData ("", "NW", "NW")>]
    let ``In a playable game, a valid move can be made to get a resulting game``
        (moves: string)  (next: string) (result: string) =
        let current = moves |> parse
        let expected = result |> parse
        let result = move current (next |> position)
        in result |> should match' expected
        
    [<Theory>]
    [<InlineData ("NW N NE", "NW N")>]
    let ``Once a move is played, it can be undone``
        (moves: string)  (previous: string) =
        let current = moves |> parse
        let expected = previous |> parse
        in
            match current with
            | Played (_, u, _) | Won (_, _, _, u) | Drawn (_, _, _, u) ->
                u.TakeBack () |> should match' expected
            | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    [<InlineData ("NW C SE N S SW NE E W", "_")>]
    let ``In a completed game, the winner can be queried``
        (moves: string)  (winner: string) =
        match moves |> parse with
        | Won (_, _, o, _) | Drawn (_, _, o, _) ->
            o.WhoWon () |> should match' winner
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", "NW", "X")>]
    [<InlineData ("NW C SE N S SW NE E W", "E", "O")>]
    [<InlineData ("NW C", "SE", "_")>]
    [<InlineData ("", "SE", "_")>]
    let ``In any game, player at can be queried``
        (moves: string)  (x: string) (player: string) =
        match moves |> parse with
        | Fresh (g, _) | Played (g, _, _) | Won (g, _, _, _)
        | Drawn (g, _, _ , _) ->
            x |> position |> g.PlayerAt |> should match' player

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", false)>]
    [<InlineData ("NW C SE N S SW NE E W", true)>]
    let ``In a filled game, is draw can be queried``
        (moves: string)  (drawn: bool) =
        match moves |> parse with
        | Won (_, Some f, _, _) | Drawn (_, f, _, _) ->
            f.IsDraw () |> should equal drawn
        | _ -> fail ()
