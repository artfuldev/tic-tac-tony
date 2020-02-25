namespace TicTacTony.Core

open System
open Helpers
open Option


type Board = internal Board of Move list

module internal Board =

    let rec playerAt x = function
        | Board (Move (y, p)::_) when x = y -> Some p
        | Board (_::ms) -> playerAt x (Board ms)
        | _ -> None

    let winner board =
        let winner = function
            | [ Some a; Some b; Some c ] ->
                if (a = b && b = c) then Some a else None
            | _ -> None
        in
            [ [ NW;  N; NE ]; [  W;  C;  E ]; [ SW;  S; SE ]
            ; [ NW;  W; SW ]; [  N;  C;  S ]; [ NE;  E; SE ]
            ; [ NW;  C; SE ]; [ NE;  C; SW ]
            ]
            |> Seq.map ((List.map (flip playerAt board)) >> winner)
            |> Seq.choose id
            |> Seq.tryHead
    
    let player = function
        | Board ms -> if List.length ms % 2 <> 1 then X else O

    let isFull = function
        | Board ms -> List.length ms = 9

    let isWon =
        winner >> isSome
    
    let positions = [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]

    let unoccupied = function
        | Board ms ->
            let occupied = flip List.contains (ms |> List.map Move.position)
            in positions |> List.filter (not << occupied)

    let make m = function
        | Board ms -> Board (m::ms)

    let undo = function
        | Board [] -> failwith "impossible" | Board (_::ms) -> Board ms

    let toString board =
        let player = flip playerAt board
        let rows = positions |> Seq.chunkBySize 3 |> Seq.map (Seq.map player)
        let row r = String.Join(" ", Seq.map (map string >> defaultValue "_") r)
        in String.Join ("\n", Seq.map row rows)
