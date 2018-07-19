open Formula
open Core

type parseState = ParseState of string * formula

let show_parseState = function
  | ParseState (s, f) -> Printf.sprintf "ParseState (%s, %s)" (s) (show_formula f)

let skipSpaces = String.strip

let explode s =
  let rec expl i l =
    if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let var =
  let rec var' t s =
    match (explode s) with
    | [] -> (t, "")
    | c :: s' ->
      if Char.is_alpha c then
        var' (t ^ String.of_char c) (String.of_char_list s')
      else
        (t, skipSpaces s)
  in
  var' ""

let null = (=) "";;

let rec parseSubFormula x =
  let open Option.Let_syntax in
  match (explode x) with
  | '(' :: s -> (
      let%bind ParseState (s', f) = parseFormula (String.of_char_list s) in
      match (explode s') with
      | ')' :: s' -> Some (ParseState (String.of_char_list s', f))
      | _ -> None)
  | '~' :: s -> (
      let%bind ParseState (s', f) = parseSubFormula (String.of_char_list s) in
      Some (ParseState (s', Not f)))
  | '[' :: ']' :: s -> (
      let%bind ParseState (s', f) = parseSubFormula (String.of_char_list s) in
      Some (ParseState (s', Box f)))
  | '<' :: '>' :: s -> (
      let%bind ParseState (s', f) = parseSubFormula (String.of_char_list s) in
      Some (ParseState (s', Diamond f)))
  | _ -> (
      let (varname, s') = var x in
      match varname with
      | "" -> None
      | "T" -> Some (ParseState (s', Top))
      | "F" -> Some (ParseState (s', Bottom))
      | _ -> Some (ParseState (s', Var (VariableIdentifier varname))))
and parseFormula s =
  let open Option.Let_syntax in
  let%bind ParseState (s', f) = parseSubFormula (skipSpaces s) in
  if null s' then
    Some (ParseState ("", f))
  else
    match (explode s') with
    | '-' :: '>' :: t -> 
      let%bind ParseState (s', g) = parseFormula (String.of_char_list t) in
      Some (ParseState ((skipSpaces s'), Imply (f, g)))
    | '/' :: '\\':: t -> 
      let%bind ParseState (s', g) = parseFormula (String.of_char_list t) in
      Some (ParseState ((skipSpaces s'), And (f, g)))
    | '\\':: '/' :: t -> 
      let%bind ParseState (s', g) = parseFormula (String.of_char_list t) in
      Some (ParseState ((skipSpaces s'), Or (f, g)))
    | '<' :: '-' :: '>' :: t -> 
      let%bind ParseState (s', g) = parseFormula (String.of_char_list t) in
      Some (ParseState ((skipSpaces s'), Equiv (f, g)))
    | _ -> Some (ParseState ((skipSpaces s'), f))

let parse s =
  let open Option.Let_syntax in                        
  let%bind ParseState(r, f) = parseFormula s in
  match r with
  | "" -> Some f
  | _ -> None
