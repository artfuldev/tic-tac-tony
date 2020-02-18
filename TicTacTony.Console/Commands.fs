namespace TicTacTony.Console

open TicTacTony.Core


type Command =
    | New
    | Move of Position
    | PlayerAt of Position
    | IsDraw
    | WhoWon
    | TakeBack
    | Exit


module Commands =

    let toShortString = function
        | Move x -> string x |> sprintf "M %-2s"
        | PlayerAt x -> string x |> sprintf "P %-2s"
        | IsDraw -> "I"
        | WhoWon -> "W"
        | TakeBack -> "T"
        | New -> "N"
        | Exit -> "E"

    let toDescription = function
        | Move x -> string x |> sprintf "Move at position %s"
        | PlayerAt x -> string x |> sprintf "Player at position %s"
        | IsDraw -> "Is the position a draw?"
        | WhoWon -> "Who won this game?"
        | TakeBack -> "Take back last move"
        | New -> "New game"
        | Exit -> "Exit"

    let toString command =
        sprintf "[%-4s] %s" (toShortString command) (toDescription command)
