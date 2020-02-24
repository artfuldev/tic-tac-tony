namespace TicTacTony.Tests

open TicTacTony.Core
open NHamcrest.Core
open Option
open Game


module Matchers =

    let match' (expected: obj) =
        CustomMatcher (
            sprintf "match %O" expected,
            fun (actual: obj) ->
                match actual, expected with
                | (:? IGame as game), (:? IGame as game') ->
                    toString game' = toString game
                | (:? Option<Player> as player), (:? string as player') ->
                    player |> map string |> defaultValue "_" |> ((=) player')
                | _ -> false
        )
    

