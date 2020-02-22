namespace TicTacTony.Core

module Helpers =

    let flip f x y = f y x
    
    let k x _ = x

    let s x y z = x z (y z)
