namespace TicTacTony.Core

open System
open Helpers
open Option
open Moves


type Board =
    internal
    | Empty
    | Has of Moves

module Board =
  
    let private _winner = function
        | [ Some a; Some b; Some c ] ->
            if (a = b && b = c) then Some a else None
        | _ -> None

    let internal winner = function
        | Empty -> None
        | Has moves ->
            [ [ NW;  N; NE ]; [  W;  C;  E ]; [ SW;  S; SE ]
            ; [ NW;  W; SW ]; [  N;  C;  S ]; [ NE;  E; SE ]
            ; [ NW;  C; SE ]; [ NE;  C; SW ]
            ]
            |> Seq.map ((List.map (flip playerAt moves)) >> _winner)
            |> Seq.choose id
            |> Seq.tryHead
    
    let internal player = function
        | Empty -> X | Has moves -> if count moves % 2 <> 1 then X else O

    let internal moves = function | Empty -> None | Has moves -> Some moves

    let internal isFull = function
        | Empty -> false | Has moves -> moves |> count |> ((=) 9)

    let internal isWon =
        winner >> isSome
    
    let positions = seq [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]

    let internal unoccupied = function
        | Empty -> positions
        | Has moves ->
            let occupied = flip Seq.contains (Moves.positions moves)
            in positions |> Seq.filter (not << occupied)

    let internal playerAt position = function
        | Empty -> None | Has moves -> playerAt position moves

    let toString board =
        let player =
            match board with
            | Empty -> k None | Has moves -> flip Moves.playerAt moves
        let rows = positions |> Seq.chunkBySize 3 |> Seq.map (Seq.map player)
        let row r =
            String.Join(" ", Seq.map (map string >> defaultValue "_") r)
        in String.Join ("\n", Seq.map row rows)
