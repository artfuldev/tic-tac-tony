namespace TicTacTony.Tests

open FsUnit
open Xunit

open TicTacTony.Core
open Board
open Matchers
open Game
open Move
open Helpers
open Option

module Tests =
    
    let private fail () = failwith "impossible"

    let private move x =
        let play p = p.Moves |> Seq.find (position >> ((=) x)) |> p.Move
        in ifPlayable play >> defaultWith fail

    let private position value = positions |> Seq.find (string >> ((=) value))

    let private parse value =
        let xs (value: string) = value.Split " " |> Seq.filter ((<>) "")
        in value |> xs |> Seq.map position |> Seq.fold (flip move) NewGame

    [<Theory>]
    [<InlineData ("NW N", "NE", "NW N NE")>]
    [<InlineData ("", "NW", "NW")>]
    let ``In a playable game, a valid move can be made to get a resulting game``
        (moves: string)  (next: string) (result: string) =
        let expected = result |> parse
        in moves |> parse |> move (next |> position) |> should match' expected
        
    [<Theory>]
    [<InlineData ("NW N NE", "NW N")>]
    let ``Once a move is played, it can be undone``
        (moves: string)  (previous: string) =
        let test u = u.TakeBack () |> should match' (previous |> parse)
        in moves |> parse |> ifUndoable test |> defaultWith fail

    [<Theory>]
    [<InlineData ("NW N SE E C", "X")>]
    [<InlineData ("NW N SE C SW S", "O")>]
    [<InlineData ("NW C SE N S SW NE E W", "_")>]
    let ``In a completed game, the winner can be queried``
        (moves: string)  (winner: string) =
        let test o = o.WhoWon () |> should match' winner
        in moves |> parse |> ifOver test |> defaultWith fail

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", "NW", "X")>]
    [<InlineData ("NW C SE N S SW NE E W", "E", "O")>]
    [<InlineData ("NW C", "SE", "_")>]
    [<InlineData ("", "SE", "_")>]
    let ``In any game, player at can be queried``
        (moves: string)  (x: string) (player: string) =
        let test g = x |> position |> g.PlayerAt |> should match' player
        in moves |> parse |> onGame test

    [<Theory>]
    [<InlineData ("NW C SE N S SW NE W E", false)>]
    [<InlineData ("NW C SE N S SW NE E W", true)>]
    let ``In a filled game, is draw can be queried``
        (moves: string)  (drawn: bool) =
        let test f = f.IsDraw () |> should equal drawn
        in moves |> parse |> ifFull test |> defaultWith fail
