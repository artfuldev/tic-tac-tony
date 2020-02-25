namespace TicTacTony.Tests

open FsCheck
open TicTacTony.Core


module private Predicates =

    let playable = function | Fresh _ | Played _ -> true | _ -> false
    
    let over = function | Won _ | Drawn _ -> true | _ -> false
    
    let undoable = function | Fresh _ -> false | _ -> true
    
    let full = function | Won (_, Some _, _, _) | Drawn _ -> true | _ -> false

    let won = function | Won _ -> true | _ -> false

    let drawn = function | Drawn _ -> true | _ -> false


module Generators =
    
    let private generator =
        let rec generate game =
            match game with
            | Fresh (_, p) | Played (_, _, p) ->
                game :: (p.Moves |> List.map p.Move |> List.collect generate)
            | _ -> [game]
        in generate Game.NewGame |> Gen.elements

    open Predicates

    type Any =
        static member Game () = Arb.fromGen generator

    type Playable =
        static member Game () = Any.Game () |> Arb.filter playable

    type Over =
        static member Game () = Any.Game () |> Arb.filter over

    type Full =
        static member Game () = Any.Game () |> Arb.filter full

    type Undoable =
        static member Game () = Any.Game () |> Arb.filter undoable

    type Won =
        static member Game () = Any.Game () |> Arb.filter won

    type Drawn =
        static member Game () = Any.Game () |> Arb.filter drawn
