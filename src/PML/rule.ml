open Formula

let modusPonens f = function
  | Imply (f', g') -> if f = f' then Some g' else None
  | _ -> None

let rec uniformSubstitute f g p =
  match f with
  | Var s -> if s = p then g else f
  | Top -> Top
  | Bottom -> Bottom
  | Not f' -> Not (uniformSubstitute f' g p)
  | Box f' -> Box (uniformSubstitute f' g p)
  | Diamond f' -> Diamond (uniformSubstitute f' g p)
  | Imply (fl, fr) -> Imply ((uniformSubstitute fl g p), (uniformSubstitute fr g p))
  | Equiv (fl, fr) -> Equiv ((uniformSubstitute fl g p), (uniformSubstitute fr g p))
  | And (fl, fr) -> And ((uniformSubstitute fl g p), (uniformSubstitute fr g p))
  | Or (fl, fr) -> Or ((uniformSubstitute fl g p), (uniformSubstitute fr g p))

let generalize f = Some (Box f)

let mp a b =
  match (a, b) with
  | (Some f, Some g) -> modusPonens f g
  | (_, _) -> None

let gn = function
  | Some f -> generalize f
  | _ -> None

let us f g v =
  match (f, g, v) with
  | (Some f, Some g, s) -> Some (uniformSubstitute f g s)
  | _ -> None