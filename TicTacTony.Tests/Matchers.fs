namespace TicTacTony.Tests

open TicTacTony.Core
open NHamcrest.Core
open Option

module Matchers =

    let match' (expected: obj) =
        CustomMatcher (
            sprintf "match %O" expected,
            fun (actual: obj) ->
                match actual, expected with
                | (:? Game as game), (:? Game as game') ->
                    Game.toString game' = Game.toString game
                | (:? Option<Player> as player), (:? string as player') ->
                    player |> map string |> defaultValue "_" |> ((=) player')
                | _ -> false
        )
    

