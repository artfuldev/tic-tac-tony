namespace TicTacTony.Tests

open FsUnit
open Xunit
open FsCheck.Xunit

open TicTacTony.Core
open Matchers
open Game
open Move
open Generators

module Tests =
    
    let private fail _ = failwith "impossible"

    let private move game =
        let f p x = p.Moves |> Seq.find (position >> ((=) x)) |> p.Move
        in match game with | Fresh (_, p) | Played (_, _, p) -> f p | _ -> fail

    let private position value = positions |> Seq.find (string >> ((=) value))

    let private parse value =
        let xs (value: string) = value.Split " " |> Seq.filter ((<>) "")
        in value |> xs |> Seq.map position |> Seq.fold move NewGame

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, a move returns a new value of game``
        (game: Game) =
        match game with
        | Fresh (_, p) | Played (_, _, p) ->
            p.Moves |> Seq.forall (p.Move >> (not << (==) game)) 
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Won> |])>]
    let ``In a won game, whoWon returns some winner`` = function
        | Won (_, _, o, _) -> o.WhoWon () <> None
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Drawn> |])>]
    let ``In a drawn game, whoWon returns none`` = function
        | Drawn (_, _, o, _) -> o.WhoWon () = None
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Full> |])>]
    let ``In a full game, isDraw returns whether the game is drawn`` = function
        | Won (_, Some f, _, _) -> f.IsDraw () = false
        | Drawn (_, f, _, _) -> f.IsDraw () = true
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, undoing a move on the game returns the same game``
        (game: Game) =
        match game with
        | Fresh (_, p) | Played (_, _, p) ->
            let undo = function
                | Played (_, u, _) | Won (_, _, _, u) | Drawn (_, _, _, u) ->
                    u.TakeBack ()
                | _ -> fail ()
            in p.Moves |> Seq.forall (p.Move >> undo >> (==) game)
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    let ``In a won game, the winner can be queried``
        (moves: string) (winner: string) =
        match moves |> parse with
        | Won (_, _, o, _) -> o.WhoWon () |> should match' winner
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", "NW", "X")>]
    [<InlineData ("NW C SE N S SW NE E W", "E", "O")>]
    [<InlineData ("NW C", "SE", "_")>]
    [<InlineData ("", "SE", "_")>]
    let ``In any game, player at can be queried``
        (moves: string) (x: string) (player: string) =
        match moves |> parse with
        | Fresh (g, _) | Played (g, _, _) | Won (g, _, _, _)
        | Drawn (g, _, _, _) ->
            x |> position |> g.PlayerAt |> should match' player
