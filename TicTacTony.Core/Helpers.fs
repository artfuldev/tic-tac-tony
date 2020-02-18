namespace TicTacTony.Core

module Helpers =
  
  let flip f x y =
    f y x

  let constant x _ =
    x

  let S x y z =
    x z (y z)

