namespace TicTacTony.Tests

open FsUnit
open Xunit
open FsCheck.Xunit

open TicTacTony.Core
open Matchers
open Game
open Generators

module Tests =
    
    let private fail _ = failwith "impossible"

    let private move game =
        let f p x = p |> moves |> List.find (position >> ((=) x)) |> make
        in match game with | Game (_, Some p, _, _, _) -> f p | _ -> fail

    let private position value = positions |> Seq.find (string >> ((=) value))

    let private parse value =
        let xs (value: string) = value.Split " " |> Seq.filter ((<>) "")
        in value |> xs |> Seq.map position |> Seq.fold move NewGame

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, a move returns a new value of game``
        (game: Game) =
        match game with
        | Game (_, Some p, _, _, _) ->
            p |> moves |> Seq.forall (make >> (not << (==) game)) 
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Won> |])>]
    let ``In a won game, whoWon returns the previous player as winner`` = function
        | Game (_, _, Some u, Some o, _) ->
            match u |> takeBack with
            | Game (_, Some p, _, _, _) -> whoWon o = Some (p |> player)
            | _ -> fail ()
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Drawn> |])>]
    let ``In a drawn game, whoWon returns none`` = function
        | Game (_, _, _, Some o, _) -> o |> whoWon = None
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Full> |])>]
    let ``In a full game, isDraw returns whether the game is drawn`` = function
        | Game (_, _, _, Some o, Some f) ->
            match o |> whoWon with
            | Some _ -> f |> (not << isDraw) | _ -> f |> isDraw
        | _ -> fail ()

    [<Property (Arbitrary = [| typeof<Playable> |])>]
    let ``In a playable game, undoing a move on the game returns the same game``
        (game: Game) =
        match game with
        | Game (_, Some p, _, _, _) ->
            let undo = function
                | Game (_, _, Some u, _, _) -> takeBack u | _ -> fail ()
            in p |> moves |> Seq.forall (make >> undo >> (==) game)
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    let ``In a won game, the winner can be queried``
        (moves: string) (winner: string) =
        match moves |> parse with
        | Game (_, _, _, Some o, _) -> o |> whoWon |> should match' winner
        | _ -> fail ()

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", "NW", "X")>]
    [<InlineData ("NW C SE N S SW NE E W", "E", "O")>]
    [<InlineData ("NW C", "SE", "_")>]
    [<InlineData ("", "SE", "_")>]
    let ``In any game, player at can be queried``
        (moves: string) (x: string) (player: string) =
        x |> position |> playerAt (moves |> parse) |> should match' player
