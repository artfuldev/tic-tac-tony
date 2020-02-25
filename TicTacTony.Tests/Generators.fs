namespace TicTacTony.Tests

open FsCheck
open TicTacTony.Core
open Game


module private Predicates =

    let playable = function | Game (_, Some _, _, _, _) -> true | _ -> false
    
    let over = function | Game (_, _, _, Some _, _) -> true | _ -> false
    
    let undoable = function | Game (_, _, Some _, _, _) -> true | _ -> false
    
    let full = function | Game (_, _, _, _, Some _) -> true | _ -> false

    let won = function
        | Game (_, _, _, Some o, _) -> whoWon o |> Option.isSome | _ -> false

    let drawn = function | Game (_, _, _, _, Some f) -> isDraw f | _ -> false

module Generators =
    
    let private generator =
        let rec generate game =
            match game with
            | Game (_, Some p, _, _, _) ->
                game :: (p |> moves |> List.map make |> List.collect generate)
            | _ -> [ game ]
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
