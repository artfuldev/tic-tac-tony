namespace TicTacTony.Console

open TicTacTony.Core
open Positions


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
        | Move x -> let p = toString x in sprintf "M %-2s" p
        | PlayerAt x -> let p = toString x in sprintf "P %-2s" p
        | IsDraw -> "I"
        | WhoWon -> "W"
        | TakeBack -> "T"
        | New -> "N"
        | Exit -> "E"

    let toDescription = function
        | Move x -> let p = toString x in sprintf "Move at position %s" p
        | PlayerAt x -> let p = toString x in sprintf "Player at position %s" p
        | IsDraw -> "Is the position a draw?"
        | WhoWon -> "Who won this game?"
        | TakeBack -> "Take back last move"
        | New -> "New game"
        | Exit -> "Exit"

    let toString command =
        sprintf "[%-4s] %s" (toShortString command) (toDescription command)
