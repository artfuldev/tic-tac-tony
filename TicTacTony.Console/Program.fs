namespace TicTacTony.Console

open TicTacTony.Core
open Game
open System

module Program =
  let rec run game =
    let ifPlayable playable =
      let _ = show game |> printfn "%s" |> ignore
      let available = available playable
      let print positions =
        let _ = printfn "Choose from the following options:"
        let choices = positions |> List.mapi (fun i position -> sprintf "%i: %s" i (Positions.toString position))
        in choices |> List.iter (printfn "%s") |> ignore
      let _ = print available
      let rec getChoice () =
        let _ = printf "Enter your choice: "
        in
          match Console.ReadLine() |> Int32.TryParse with
          | (true, choice) when choice < (List.length available) && choice >= 0 -> choice
          | _ -> let _ = printfn "Couldn't find choice. Try again." in getChoice ()
      let choice = getChoice ()
      let position = List.item choice available
      in
        match move position playable with
        | Ok game -> run game
        | Error _ -> let _ = printfn "Error occurred. Starting a new game..." in run newGame
    let ifComplete _ =
      let _ = show game |> printfn "%s" |> ignore
      let _ = printfn "Enter anything to play again or nothing to exit"
      in
        if Console.ReadLine () |> String.IsNullOrWhiteSpace
        then printfn "Exiting..." |> ignore
        else let _ = printfn "Starting a new game..." in run newGame
    in Game.fold ifPlayable ifComplete game
  
  [<EntryPoint>]
  let main argv =
    let _ = printfn "Welcome to TicTacTony!\nStarting a new game..."
    let _ = run newGame |> ignore
    in 0
