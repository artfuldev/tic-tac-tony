namespace TicTacTony.Console

open Commands
open Parser

module Reader =
  open System
  let read options =
    let _ = printfn "Choose from:"
    let _ = options |> Seq.map toString |> Seq.iter (printfn "%s")
    let rec _read allowed =
      let _ = printfn "Enter your choice here, without [], eg, for player at NW, type P NW"
      let retry () = let _ = printfn "invalid command, try again" in _read allowed
      in
        match Console.ReadLine () |> parse with
        | None -> retry ()
        | Some command -> if allowed |> List.contains command then command else retry ()
    in _read options

