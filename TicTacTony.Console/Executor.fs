namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader
open System


module Executor =
  
    open Move

    let private moves p =
        p.Moves |> Seq.map (position >> Move >> Some)
  
    let private options f o u p =
        seq [
            defaultValue Seq.empty (map moves p)
            Board.positions |> Seq.map (PlayerAt >> Some)
            seq [
                (map (fun (f: IFull) -> IsDraw) f)
                (map (fun (o: IOver) -> WhoWon) o)
                (map (fun (u: IUndoable) -> TakeBack) u)
                Some New; Some Exit
            ]
        ]
        |> Seq.concat
        |> Seq.choose id

    let private playerAt p g =
        let sprint = sprintf "The cell at %s is %s" (string p)
        let suffix =
            match g.PlayerAt p with
            | Some x -> sprintf "filled by %s" (string x)
            | None -> "empty"
        in sprint suffix

    let private whoWon o =
        match o.WhoWon() with
        | Some x -> sprintf "The winner is %s." (string x)
        | None -> "Nobody won."

    let private isDraw f =
        if f.IsDraw () then "It's a draw." else "It's not a draw."

    let private takeBack u =
        u.TakeBack ()

    let private move x p =
        p.Moves |> Seq.find (position >> ((=) x)) |> p.Move

    let rec private step game g f o u p =
        let _ = Board.toString g.Board |> printfn "\n%s"
        let command = read (options f o u p)
        in handle game g f o u p command

    and private handle game g f o u p command =
        let print =
            printfn "%s\nPress any key to proceed..."
            >> Console.ReadKey >> ignore
        let current f x = let _ = iter (f >> print) x in game
        let next f x = map f x |> defaultValue game
        let _ = printfn "%s" (Commands.toDescription command)
        in
            match command with
            | New -> Game.NewGame |> play
            | Move x -> p |> next (move x) |> play
            | PlayerAt x -> Some g |> current (playerAt x) |> play
            | IsDraw -> f |> current isDraw |> play
            | WhoWon -> o |> current whoWon |> play
            | TakeBack -> u |> next takeBack |> play
            | Exit -> exit 0

    and play game =
        match game with
        | Fresh (g, p) -> step game g None None None (Some p)
        | Played (g, u, p) -> step game g None None (Some u) (Some p)
        | Won (g, f', o, u) -> step game g f' (Some o) (Some u) None
        | Drawn (g, f, o, u) -> step game g (Some f) (Some o) (Some u) None
