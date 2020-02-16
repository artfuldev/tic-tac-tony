namespace TicTacTony.Console

open TicTacTony.Core
open Option
open TicTacTony.Console
open Reader

module Executor =
  
  let private options filled completed undoable playable = function
    | Some { Board = board } ->
      [
        defaultValue [] (map (fun (p: IPlayable) -> Board.unoccupied board |> List.map (Move >> Some)) playable)
        Positions.all |> List.map (PlayerAt >> Some)
        [ map (fun (f: IFilled) -> IsDraw) filled ]
        [ map (fun (c: ICompleted) -> WhoWon) completed ]
        [ map (fun (u: IUndoable) -> TakeBack) undoable ]
        [ Some New; Some Exit ]
      ]
      |> List.concat
      |> List.choose id
    | _ -> [ New; Exit ]

  open Game
  let rec private _execute filled completed undoable playable game game' =
    let impossible () = printfn "impossible!" |> exit 1
    let iff f x = match x with | Some x -> f x | None -> impossible ()
    let _ = match game with | Some game -> printfn "\n%s" (Board.print game.Board) | None -> ()
    let _ = match completed with | Some completed -> printfn "Winner: %O" (completed.WhoWon ()) | None -> ()
    let _ = match filled with | Some filled -> printfn "IsDrawn: %O" (filled.IsDraw ()) | None -> ()
    let command = read (options filled completed undoable playable game)
    let _ = printfn "%s: " (Commands.toDescription command)
    in
      match command with
      | New -> execute NewGame
      | Move position -> iff (fun playable -> execute (playable.Move position)) playable
      | PlayerAt position -> iff (fun g -> let _ = printfn "%O" (g.PlayerAt position) in execute game') game
      | IsDraw -> iff (fun f -> let _ = printfn "%O" (f.IsDraw ()) in execute game') filled
      | WhoWon -> iff (fun c -> let _ = printfn "%O" (c.WhoWon ()) in execute game') completed
      | TakeBack -> iff (fun u -> execute (u.TakeBack ())) undoable
      | Exit -> exit 0
  and private execute game =
    match game with
    | Begun (g, p) -> _execute None None None (Some p) (Some g) game
    | InProgress (g, u, p) -> _execute None None (Some u) (Some p) (Some g) game
    | Won (g, f', c, u) -> _execute f' (Some c) (Some u) None (Some g) game
    | Drawn (g, f, c, u) -> _execute (Some f) (Some c) (Some u) None (Some g) game

  let start () = execute NewGame
