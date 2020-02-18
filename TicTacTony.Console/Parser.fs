namespace TicTacTony.Console

open TicTacTony.Core.Board
open System.Text.RegularExpressions
open Option


module Parser =
  
    let (|Regex|_|) pattern input =
        let result = Regex.Match(input, pattern)
        in 
            if result.Success
            then [ for g in result.Groups -> g.Value ] |> List.tail |> Some
            else None

    let private _pos value = positions |> Seq.tryFind (string >> ((=) value))

    let parse = function
        | Regex "^M (NW|N|NE|W|C|E|SW|S|SE)$" [x] -> x |> _pos |> map Move
        | Regex "^P (NW|N|NE|W|C|E|SW|S|SE)$" [x] -> x |> _pos |> map PlayerAt
        | Regex "^I$" _ -> Some IsDraw
        | Regex "^W$" _ -> Some WhoWon
        | Regex "^T$" _ -> Some TakeBack
        | Regex "^E$" _ -> Some Exit
        | Regex "^N$" _ -> Some New
        | _ -> None
