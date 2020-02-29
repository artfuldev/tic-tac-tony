namespace TicTacTony.Tests

open FsCheck
open TicTacTony.Core
open Game


module private Predicates =
    
    type Predicate = IGame -> bool

    let playable : Predicate = function | :? IPlayable -> true | _ -> false
    
    let over : Predicate = function | :? IOver -> true | _ -> false
    
    let undoable : Predicate = function | :? IUndoable -> true | _ -> false
    
    let full : Predicate = function | :? IFull -> true | _ -> false

    let won : Predicate = function
        | :? IOver as o -> whoWon o |> Option.isSome | _ -> false

    let drawn : Predicate = function | :? IFull as f -> isDraw f | _ -> false

module Generators =
    
    let private generator =
        let rec generate (game: IGame) =
            match game with
            | :? IPlayable as p ->
                let moves = p |> moves
                in  game :: (moves |> List.map make |> List.collect generate)
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
