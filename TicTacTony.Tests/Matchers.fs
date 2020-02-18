namespace TicTacTony.Tests

open TicTacTony.Core
open NHamcrest.Core


module Matchers =
    
    let private board game =
        match game with
        | Fresh (g, _) | Played (g, _, _)  | Won (g, _, _, _)
        | Drawn (g, _, _, _) -> g.Board

    let beGame game =
        CustomMatcher (
            sprintf "be game %O" game,
            fun (game': obj) ->
                match game' with
                | :? Game as game'-> board game' = board game
                | _ -> false
        )

    let bePlayer player =
        CustomMatcher (
            sprintf "be player %s" player,
            fun (player': obj) ->
                match player' with
                | :? Option<Player> as player' ->
                    player'
                    |> Option.map Player.toString
                    |> Option.defaultValue "_"
                    |> ((=) player)
                | _ -> false
        )
    

