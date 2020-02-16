namespace TicTacTony.Console

open TicTacTony.Core.Positions

module Parser =

  open System.Text.RegularExpressions
  let (|Regex|_|) pattern input =
    let result = Regex.Match(input, pattern)
    in 
      if result.Success
      then [ for g in result.Groups -> g.Value ] |> List.tail |> Some
      else None

  let private _position value =
    all |> List.tryFind (toString >> ((=) value))

  let parse = function
    | Regex "^M (NW|N|NE|W|C|E|SW|S|SE)$" [position] -> Option.map Move (_position position)
    | Regex "^P (NW|N|NE|W|C|E|SW|S|SE)$" [position] -> Option.map PlayerAt (_position position)
    | Regex "^I$" _ -> Some IsDraw
    | Regex "^W$" _ -> Some WhoWon
    | Regex "^T$" _ -> Some TakeBack
    | Regex "^E$" _ -> Some Exit
    | Regex "^N$" _ -> Some New
    | _ -> None
