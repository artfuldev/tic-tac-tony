namespace TicTacTony.Core


type Position =
    | NW |  N | NE
    |  W |  C |  E
    | SW |  S | SE

module Position =
    
    let all = [ NW;  N; NE;  W;  C;  E; SW;  S; SE ]

type Player = X | O

module Player =
    
    let other = function | X -> O | O -> X
    
