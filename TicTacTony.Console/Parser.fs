namespace TicTacTony.Console

open TicTacTony.Core
open System.Text.RegularExpressions
open Game
open TicTacTony.Console


module Parser =
  
    let (|Regex|_|) pattern input =
        let result = Regex.Match(input, pattern)
        in 
            if result.Success
            then [ for g in result.Groups -> g.Value ] |> List.tail |> Some
            else None

    let parse game = function
        | Regex "^M (NW|N|NE|W|C|E|SW|S|SE)$" [x] ->
            match game with
            | Game (_, Some p, _, _, _) ->
                let position = position >> string >> (=) x
                in p |> moves |> List.tryFind position |> Option.map Play
            | _ -> None
        | Regex "^P (NW|N|NE|W|C|E|SW|S|SE)$" [x] ->
            positions
            |> List.tryFind (string >> (=) x)
            |> Option.map (fun x -> PlayerAt (x, game))
        | Regex "^I$" _ ->
            match game with
            | Game (_, _, _, _, Some f) -> IsDraw f |> Some | _ -> None
        | Regex "^W$" _ ->
            match game with
            | Game (_, _, _, Some o, _) -> WhoWon o |> Some | _ -> None
        | Regex "^T$" _ ->
            match game with
            | Game (_, _, Some u, _, _) -> TakeBack u |> Some | _ -> None
        | Regex "^N$" _ -> Some New
        | Regex "^E$" _ -> Some Exit
        | _ -> None
