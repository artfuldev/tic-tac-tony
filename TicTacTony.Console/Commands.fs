namespace TicTacTony.Console

open TicTacTony.Core


type Command =
    | New
    | Play of Move
    | PlayerAt of Position * Game
    | IsDraw of IFull
    | WhoWon of IOver
    | TakeBack of IUndoable
    | Exit

module Commands =

    let toShortString = function
        | Play x -> x |> Game.position |> string |> sprintf "M %-2s"
        | PlayerAt (x, _) -> string x |> sprintf "P %-2s"
        | IsDraw _ -> "I"
        | WhoWon _ -> "W"
        | TakeBack _ -> "T"
        | New -> "N"
        | Exit -> "E"

    let toDescription = function
        | Play x -> string x |> sprintf "Move at position %s"
        | PlayerAt (x, _) -> string x |> sprintf "Player at position %s"
        | IsDraw _ -> "Is the position a draw?"
        | WhoWon _ -> "Who won this game?"
        | TakeBack _ -> "Take back last move"
        | New -> "New game"
        | Exit -> "Exit"

    let toString command =
        sprintf "[%-4s] %s" (toShortString command) (toDescription command)
