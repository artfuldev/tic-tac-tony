namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader
open System

module Executor =
  
  let private options g f c u p =
    [
      defaultValue [] (map (fun (p: IPlayable) -> p.Moves |> List.map (Move.position >> Move >> Some)) p)
      g.Positions |> List.map (PlayerAt >> Some)
      [ map (fun (f: IFilled) -> IsDraw) f ]
      [ map (fun (c: ICompleted) -> WhoWon) c ]
      [ map (fun (u: IUndoable) -> TakeBack) u ]
      [ Some New; Some Exit ]
    ]
    |> List.concat
    |> List.choose id

  let private playerAt p g =
    let sprint = sprintf "The cell at %s is %s" (Positions.toString p)
    let suffix = match g.PlayerAt p with | Some x -> sprintf "filled by %s" (Player.toString x) | None -> "empty"
    in sprint suffix

  let private whoWon c =
    match c.WhoWon() with | Some x -> sprintf "The winner is %s." (Player.toString x) | None -> "Nobody won."

  let private isDraw f =
    if f.IsDraw () then "It's a draw." else "It's not a draw."

  let private takeBack u =
    u.TakeBack ()

  let private move x p =
    p.Moves |> List.find (Move.position >> ((=) x)) |> p.Move

  let rec private step game g f c u p =
    let _ = Board.toString g.Board |> printfn "\n%s"
    let command = read (options g f c u p)
    in handle game g f c u p command

  and private handle game g f c u p command =
      let print = printfn "%s\nPress any key to proceed..." >> Console.ReadKey >> ignore
      let current f x = let _ = iter (f >> print) x in game
      let next f x = map f x |> defaultValue game
      let _ = printfn "%s" (Commands.toDescription command)
      in
        match command with
        | New -> Game.NewGame |> play
        | Move x -> p |> next (move x) |> play
        | PlayerAt x -> Some g |> current (playerAt x) |> play
        | IsDraw -> f |> current isDraw |> play
        | WhoWon -> c |> current whoWon |> play
        | TakeBack -> u |> next takeBack |> play
        | Exit -> exit 0

  and play game =
    match game with
    | Begun (g, p) -> step game g None None None (Some p)
    | InProgress (g, u, p) -> step game g None None (Some u) (Some p)
    | Won (g, f', c, u) -> step game g f' (Some c) (Some u) None
    | Drawn (g, f, c, u) -> step game g (Some f) (Some c) (Some u) None
