namespace TicTacTony.Core

open System
open Helpers
open Option

type Board = Map<Position, Player>

module internal Board =

    let rec playerAt x (b: Board) = Map.tryFind x b
    
    let make x p (b: Board) = Map.add x p b

    let winner b =
        let winner = function
            | [ Some a; Some b; Some c ] ->
                if (a = b && b = c) then Some a else None
            | _ -> None
        in
            [ [ NW;  N; NE ]; [  W;  C;  E ]; [ SW;  S; SE ]
            ; [ NW;  W; SW ]; [  N;  C;  S ]; [ NE;  E; SE ]
            ; [ NW;  C; SE ]; [ NE;  C; SW ]
            ]
            |> Seq.map ((List.map (flip playerAt b)) >> winner)
            |> Seq.choose id
            |> Seq.tryHead

    let isFull b = Map.count b = 9

    let isWon = winner >> isSome
    
    let positions = [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]

    let unoccupied b = positions |> List.filter (not << flip Map.containsKey b)

    let toString board =
        let player = flip playerAt board
        let rows = positions |> Seq.chunkBySize 3 |> Seq.map (Seq.map player)
        let row r = String.Join(" ", Seq.map (map string >> defaultValue "_") r)
        in String.Join ("\n", Seq.map row rows)
