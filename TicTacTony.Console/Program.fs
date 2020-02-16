namespace TicTacTony.Console

module Program =
  [<EntryPoint>]
  let main argv =
    printfn "Welcome to TicTacTony!!!"
    Executor.start ()
