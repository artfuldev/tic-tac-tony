namespace TicTacTony.Core


type IBoard =
    abstract member ToMap: unit -> Map<Position, Player>

module internal Board =
    
    let play player position (board: IBoard) =
        board.ToMap () |> Map.add position player

    let free (board: IBoard) position =
        board.ToMap () |> Map.containsKey position |> not

    let private _playerAt position map =
        Map.tryFind position map

    let playerAt position (board: IBoard) =
        board.ToMap () |> _playerAt position

    let private _winner map =
        let winner = function
        | [ Some a; Some b; Some c ] ->
            if (a = b && b = c) then Some a else None
        | _ -> None
        in
        [ [ NW;  N; NE ]; [  W;  C;  E ]; [ SW;  S; SE ]
        ; [ NW;  W; SW ]; [  N;  C;  S ]; [ NE;  E; SE ]
        ; [ NW;  C; SE ]; [ NE;  C; SW ]
        ]
        |> Seq.map (List.map (Helpers.flip _playerAt map) >> winner)
        |> Seq.choose id
        |> Seq.tryHead
    
    let winner (board: IBoard) =
        board.ToMap () |> _winner

    let isWin player position (board: IBoard) =
        board.ToMap () |> Map.add position player |> _winner |> Option.isSome

    open System
    open Option
    
    let positions = seq [ NW; N; NE; W; C; E; SW; S; SE ]

    let toString (board: IBoard) =
        let playerAt = Helpers.flip playerAt board
        let rows = positions |> Seq.chunkBySize 3 |> Seq.map (Seq.map playerAt)
        let row r = String.Join(" ", Seq.map (map string >> defaultValue "_") r)
        in String.Join ("\n", Seq.map row rows)
