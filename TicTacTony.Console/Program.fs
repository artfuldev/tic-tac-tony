namespace TicTacTony.Console

open Executor
open TicTacTony.Core.Game


module Program =
    
    [<EntryPoint>]
    let main _ =
        printfn "Welcome to TicTacTony!!!"
        play NewGame
