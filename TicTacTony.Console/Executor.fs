namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader

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

  let private winner = function
    | Some x -> sprintf "The winner is %s." (Player.toString x)
    | None -> "Nobody won."

  let private drawn = function
    | true -> "It's a draw."
    | false -> "It's not a draw."

  let rec private step game g f c u p =
    let print = printfn "%s"
    let _ = print "\n"
    let _ = Board.toString g.Board |> print
    let _ = iter (fun f -> (f.IsDraw () |> drawn) |> print) f
    let _ = iter (fun c -> (c.WhoWon () |> winner) |> print) c
    let command = read (options g f c u p)
    in handle game g f c u p command

  and private handle game g f c u p command =
      let impossible () = printfn "impossible!" |> exit 1
      let iff f x = match x with | Some x -> f x | None -> impossible ()
      let _ = printfn "%s" (Commands.toDescription command)
      in
        match command with
        | New -> play Game.NewGame
        | Move p' -> iff (fun p -> p.Moves |> List.find (Move.position >> ((=) p')) |> p.Move |> play) p
        | PlayerAt position -> let _ = printfn "%O" (g.PlayerAt position) in play game
        | IsDraw -> iff (fun f -> let _ = printfn "%O" (f.IsDraw ()) in play game) f
        | WhoWon -> iff (fun c -> let _ = printfn "%O" (c.WhoWon ()) in play game) c
        | TakeBack -> iff (fun u -> play (u.TakeBack ())) u
        | Exit -> exit 0

  and play game =
    match game with
    | Begun (g, p) -> step game g None None None (Some p)
    | InProgress (g, u, p) -> step game g None None (Some u) (Some p)
    | Won (g, f', c, u) -> step game g f' (Some c) (Some u) None
    | Drawn (g, f, c, u) -> step game g (Some f) (Some c) (Some u) None
