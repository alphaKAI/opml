open Formula
open Parser
open Axiom
open Core

type indicator =
  | INum of int
  | IStr of string
  | IPre
[@@deriving show]

type command =
  | Axiom of formula
  | MP of indicator * indicator
  | US of indicator * formula * string
  | G of indicator
  | Name of indicator * string
  | Q
  | Comment of string

let show_command = function
  | Axiom f -> Printf.sprintf "Axiom: %s" (show_formula f)
  | MP (i, j) -> Printf.sprintf "MP: %s, %s" (show_indicator i) (show_indicator j)
  | US (i, f, s) -> Printf.sprintf "US: %s, %s, %s" (show_indicator i) (show_formula f) s
  | G i -> Printf.sprintf "G: %s" (show_indicator i)
  | Name (i, s) -> Printf.sprintf "Name: %s, %s" (show_indicator i) s
  | Q -> "Q"
  | Comment s -> Printf.sprintf "Comment: %s" s

module SFMap = Map.Make(String)

type proved =
  | Proved of formula list * formula SFMap.t

let emptyEnv () = Proved ([], SFMap.empty)

let axiom s = 
  match Parser.parseFormula (Parser.skipSpaces s) with
  | Some (Parser.ParseState ("", f)) -> Some (Axiom f)
  | _ -> None

let char_as_int x = (int_of_char x) - 48

let rec readNum n s =
  match (explode s) with
  | c :: s' ->
    if Char.is_digit c then
      readNum (n*10 + (char_as_int c)) (String.of_char_list s')
    else
      (n, s)
  | [] -> (n, "")

let rec readVar v s =
  match (explode s) with
  | c :: s' ->
    if Char.is_alpha c
    then readVar (v ^ String.of_char c) (String.of_char_list s')
    else (v, s)
  | [] -> (v, "") 

let rec indicator_f x =
  match (explode x) with
  | ' ' :: s -> indicator_f (String.of_char_list s)
  | '#' :: '^' :: s -> Some (IPre, Parser.skipSpaces (String.of_char_list s))
  | '#' :: c :: r -> (
      let s = c :: r |> String.of_char_list in
      match (Char.is_digit c, Char.is_alpha c) with
      | (true, false) -> let (n, s') = readNum 0 s in Some (INum n, Parser.skipSpaces s')
      | (false, true) -> let (var, s') = readVar "" s in Some (IStr var, Parser.skipSpaces s')
      | _ -> None)
  | _ -> None

let mp s =
  match (indicator_f s) with
  | Some (i, s') -> (match indicator_f s' with
      | Some (j, "") -> Some (MP (i, j))
      | _ -> None)
  | None -> None


let us s =
  match  (indicator_f s) with
  | Some (i, s') -> (
      match (Parser.parseFormula s') with
      | Some (Parser.ParseState (s', f)) -> (
          match (Parser.var s') with
          | ("", _) -> None
          | (v, "") -> Some (US (i, f, v))
          | _ -> None)
      | None -> None)
  | None -> None

let gn s =
  match (indicator_f s) with
  | Some (i, "") -> Some (G (i))
  | _ -> None


let name s =
  match (indicator_f s) with
  | Some(i, s') ->
    let (v, s'') = Parser.var s' in
    if not (null v) && null s'' then
      Some (Name (i, v))
    else
      None
  | None -> None

let quit s = if null (Parser.skipSpaces s) then Some Q else None

let comment x = Some (Comment x)

let cparse' s =
  List.map ~f:(fun (prefix, f) ->
      match String.chop_prefix ~prefix s with
      | Some s' -> f s'
      | None -> None)
    [("Axiom", axiom); ("MP", mp); ("US", us); ("G", gn); ("Name", name); ("Q", quit); ("//", comment)] 

let cparse s =
  let f = (fun c d ->
      match c with
      | Some x -> Some x
      | None -> d) in
  List.fold ~f ~init:(None) (cparse' s)

let getFormula indi prov =
  match (indi, prov) with
  | (IPre, (Proved (f :: _, _))) -> Some f
  | (IPre, (Proved ([], _))) -> None
  | (IStr key, Proved (_, m)) -> SFMap.find m key
  | (INum i, Proved (thms, _)) ->
    if 0 <= i && i < List.length thms then
      Some (List.nth_exn thms (List.length thms - i - 1))
    else
      None

let run cmd prov =
  match (cmd, prov) with
  | (Axiom f, Proved (thms, m)) ->
    if isAxiom f then
      Some (Proved (f :: thms, m))
    else
      None
  | (MP (f, fg), Proved (thms, m)) -> (
      let env = Proved (thms, m) in
      match Rule.mp (getFormula f env) (getFormula fg env) with
      | Some f' -> Some (Proved ((f' :: thms), m))
      | None -> None)
  | (G f, Proved (thms, m)) -> (
      let env = Proved (thms, m) in
      match Rule.gn (getFormula f env) with
      | Some f' -> Some (Proved ((f' :: thms), m))
      | None -> None)
  | (US (f, g, p), Proved (thms, m)) -> (
      let env = Proved (thms, m) in
      match Rule.us (getFormula f env) (Some g) (VariableIdentifier p) with
      | Some f' -> Some (Proved ((f' :: thms), m))
      | None -> None)
  | (Name (f, s), Proved (thms, m)) -> (
      let env = Proved (thms, m) in
      match (getFormula f env) with
      | Some f' -> 
        (match (SFMap.find m s) with
         | None -> Some (Proved (thms,
                                 match (SFMap.add m ~key:s ~data:f') with
                                 | `Ok x -> x
                                 | `Duplicate -> m))
         | Some _ -> None)
      | _ -> None)
  | (Q, _) | (Comment _, _) -> None

