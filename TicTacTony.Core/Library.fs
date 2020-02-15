namespace TicTacTony.Core

module Domain =
  type Player =
    | X
    | O

  type CellState =
    | Empty
    | FilledBy of Player

  type HorizontalPosition =
    | Left
    | Center
    | Right

  type VerticalPosition =
    | Top
    | Middle
    | Bottom

  type CellPosition =
    | CellPosition of HorizontalPosition * VerticalPosition

  type Cell =
    | Cell of CellPosition * CellState
  let position = function
    | Cell (position, _) -> position
  let player = function
  | Cell (_, FilledBy player) -> Some player
  | _ -> None
  let private has position' =
    Seq.map position >> Seq.exists ((=) position')
  let private filledBy player' =
    Seq.filter (fun cell -> cell |> player |> Option.isSome && cell |> player |> Option.get |> (=) player')

  type XPlayableBoard =
    | Empty
    | TwoFilled of Cell list
    | FourFilled of Cell list
    | SixFilled of Cell list
    | EightFilled of Cell list

  type OPlayableBoard =
    | OneFilled of Cell
    | ThreeFilled of Cell list
    | FiveFilled of Cell list
    | SevenFilled of Cell list
      
  type XWonBoard =
    | FiveFilled of Cell list
    | SevenFilled of Cell list
    | NineFilled of Cell list
      
  type OWonBoard =
    | SixFilled of Cell list
    | EightFilled of Cell list

  type Board =
    | XPlayable of XPlayableBoard
    | OPlayable of OPlayableBoard
    | XWon of XWonBoard
    | OWon of OWonBoard
    | Drawn of Cell list

  let winCombinations = List.map (List.map CellPosition) [
    [(Left, Top); (Center, Top); (Right, Top)];
    [(Left, Middle); (Center, Middle); (Right, Middle)];
    [(Left, Bottom); (Center, Bottom); (Right, Bottom)];
    [(Left, Top); (Left, Middle); (Left, Bottom)];
    [(Center, Top); (Center, Middle); (Center, Bottom)];
    [(Right, Top); (Right, Middle); (Right, Bottom)];
    [(Left, Top); (Center, Middle); (Right, Bottom)];
    [(Left, Bottom); (Center, Middle); (Right, Top)];
  ]

  let hasWon player cells =
    winCombinations |> List.exists (List.forall (fun p -> cells |> filledBy player |> Seq.map position |> Seq.contains p))

  let ifWon player onTrue onFalse cells =
    if hasWon player cells then onTrue cells else onFalse cells

  let iff predicate result =
    if predicate then Some result else None

  let moveX playable position' =
    let cell = Cell (position', FilledBy X)
    let canPlay = not << has position'
    let ifWon = ifWon X
    in
      match playable with
      | Empty -> OneFilled cell |> OPlayable |> Some
      | TwoFilled cells -> iff (canPlay cells) (ThreeFilled (cell::cells) |> OPlayable)
      | FourFilled cells ->
        iff
          (canPlay cells)
          (ifWon (FiveFilled >> XWon) (OPlayableBoard.FiveFilled >> OPlayable) (cell::cells))
      | XPlayableBoard.SixFilled cells ->
        iff
          (canPlay cells)
          (ifWon (SevenFilled >> XWon) (OPlayableBoard.SevenFilled >> OPlayable) (cell::cells))
      | XPlayableBoard.EightFilled cells ->
        iff
          (canPlay cells)
          (ifWon (NineFilled >> XWon) (Drawn) (cell::cells))

  let moveO playable position' =
    let cell' = Cell (position', FilledBy O)
    let canPlay = not << has position'
    let ifWon = ifWon O
    let played = XPlayable
    in
      match playable with
      | OneFilled cell -> iff (canPlay [cell]) (TwoFilled (cell::[cell']) |> played)
      | ThreeFilled cells -> iff (canPlay cells) (FourFilled (cell'::cells) |> played)
      | OPlayableBoard.FiveFilled cells ->
        iff
          (canPlay cells)
          (ifWon (SixFilled >> OWon) (XPlayableBoard.SixFilled >> played) (cell'::cells))
      | OPlayableBoard.SevenFilled cells ->
        iff
          (canPlay cells)
          (ifWon (EightFilled >> OWon) (XPlayableBoard.EightFilled >> played) (cell'::cells))
