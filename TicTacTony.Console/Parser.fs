namespace TicTacTony.Console

open TicTacTony.Core
open System.Text.RegularExpressions
open Option
open TicTacTony.Console
open Game


module Parser =
  
    let (|Regex|_|) pattern input =
        let result = Regex.Match(input, pattern)
        in 
            if result.Success
            then [ for g in result.Groups -> g.Value ] |> List.tail |> Some
            else None

    let pos x = Game.positions |> Seq.tryFind (string >> ((=) x))

    let parse (game: IGame) = function
        | Regex "^M (NW|N|NE|W|C|E|SW|S|SE)$" [x] ->
            match game with
            | :? IPlayable as p ->
                let move x = p.Moves |> Seq.tryFind (position >> (=) x)
                in x |> pos |> map move |> flatten |> map (fun m -> Play (m, p))
            | _ -> None
        | Regex "^P (NW|N|NE|W|C|E|SW|S|SE)$" [x] ->
            x |> pos |> map (fun x -> PlayerAt (x, game))
        | Regex "^I$" _ ->
            match game with | :? IFull as f -> IsDraw f |> Some | _ -> None
        | Regex "^W$" _ ->
            match game with | :? IOver as o -> WhoWon o |> Some | _ -> None
        | Regex "^T$" _ ->
            match game with
            | :? IUndoable as u -> TakeBack u |> Some | _ -> None
        | Regex "^N$" _ -> Some New
        | Regex "^E$" _ -> Some Exit
        | _ -> None
