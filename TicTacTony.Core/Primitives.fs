namespace TicTacTony.Core


type Position =
    | NW |  N | NE
    |  W |  C |  E
    | SW |  S | SE

type Player = X | O

type Move = private Move of Position * Player

module Move =
    
    let position = function Move (position, _) -> position
