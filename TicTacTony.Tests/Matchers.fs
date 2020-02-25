namespace TicTacTony.Tests

open TicTacTony.Core
open NHamcrest.Core
open Option


module Matchers =

    let ( == ) game game' =
        Game.toString game = Game.toString game'

    let match' (expected: obj) =
        CustomMatcher (
            sprintf "match %O" expected,
            fun (actual: obj) ->
                match actual, expected with
                | (:? Option<Player> as player), (:? string as player') ->
                    player |> map string |> defaultValue "_" |> ((=) player')
                | _ -> false
        )
