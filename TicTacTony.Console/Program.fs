namespace TicTacTony.Console

open Executor
open TicTacTony.Core


module Program =
    
    [<EntryPoint>]
    let main _ =
        let _ = printfn "Welcome to TicTacTony!!!"
        in play <| Game.NewGame ()
