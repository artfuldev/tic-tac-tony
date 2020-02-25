namespace TicTacTony.Console

open TicTacTony.Core
open System.Text.RegularExpressions
open Option
open Game
open TicTacTony.Console
open Move


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
            | Fresh (_, p) | Played (_, _, p) ->
                p.Moves
                |> Seq.tryFind (position >> string >> ((=) x))
                |> map (fun x -> Play (x, p))
            | _ -> None
        | Regex "^P (NW|N|NE|W|C|E|SW|S|SE)$" [x] ->
            match game with
            | Fresh (g, _) | Played (g, _, _) | Won (g, _, _, _)
            | Drawn (g, _, _, _) ->
                positions
                |> Seq.tryFind (string >> ((=) x))
                |> map (fun x -> PlayerAt (x, g))
        | Regex "^I$" _ ->
            match game with
            | Won (_, Some f, _, _) | Drawn (_, f, _, _) -> IsDraw f |> Some
            | _ -> None
        | Regex "^W$" _ ->
            match game with
            | Won (_, _, o, _) | Drawn (_, _, o, _) -> WhoWon o |> Some
            | _ -> None
        | Regex "^T$" _ ->
            match game with
            | Played (_, u, _) | Won (_, _, _, u) | Drawn (_, _, _, u) ->
                TakeBack u |> Some
            | _ -> None
        | Regex "^N$" _ -> Some New
        | Regex "^E$" _ -> Some Exit
        | _ -> None
