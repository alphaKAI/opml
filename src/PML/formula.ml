open Sexplib.Conv
open Core

type variableIdentifier = VariableIdentifier of string [@@deriving eq, ord, sexp]

let show_variableIdentifier = function
  | VariableIdentifier x -> x;;

type formula =
  | Var of variableIdentifier
  | Top
  | Bottom
  | Not of formula
  | Box of formula
  | Diamond of formula
  | Imply of formula * formula
  | Equiv of formula * formula
  | And of formula * formula
  | Or of formula * formula
[@@derving show, eq]

let rec show_formula = function
  | Var vi -> show_variableIdentifier vi
  | Top -> "T"
  | Bottom -> "F"
  | Not f -> "~" ^ (show_formula f)
  | Box f -> "[]" ^ (show_formula f)
  | Diamond f -> "<>" ^ (show_formula f)
  | Imply (f, g) -> "(" ^ (show_formula f) ^ "->" ^ (show_formula g) ^ ")"
  | Equiv (f, g) -> "(" ^ (show_formula f) ^ "<->" ^ (show_formula f) ^ ")"
  | And (f, g) -> "(" ^ (show_formula f) ^ "/\\" ^ (show_formula g) ^ ")"
  | Or (f, g) -> "(" ^ (show_formula f) ^ "\\/" ^ (show_formula g) ^ ")"

let rec isPropositionalFormula = function
  | Var _ -> true
  | Top -> true
  | Bottom -> true
  | Not f -> isPropositionalFormula f
  | Box _ -> false
  | Diamond _ -> false
  | Imply (f, g) -> isPropositionalFormula f && isPropositionalFormula g
  | Equiv (f, g) -> isPropositionalFormula f && isPropositionalFormula g
  | And (f, g) -> isPropositionalFormula f && isPropositionalFormula g
  | Or (f, g) -> isPropositionalFormula f && isPropositionalFormula g

module VISet = Set.Make(struct
    type t = variableIdentifier
    let compare = compare_variableIdentifier

    let t_of_sexp = variableIdentifier_of_sexp
    let sexp_of_t = sexp_of_variableIdentifier
  end)

let rec getPropositionalLetters = function
  | Var p -> VISet.singleton p
  | Top -> VISet.empty
  | Bottom -> VISet.empty
  | Not f -> getPropositionalLetters f
  | Box f -> getPropositionalLetters f
  | Diamond f -> getPropositionalLetters f
  | Imply (f, g) -> VISet.union (getPropositionalLetters f) (getPropositionalLetters g)
  | Equiv (f, g) -> VISet.union (getPropositionalLetters f) (getPropositionalLetters g)
  | And (f, g) -> VISet.union (getPropositionalLetters f) (getPropositionalLetters g)
  | Or (f, g) -> VISet.union (getPropositionalLetters f) (getPropositionalLetters g)

module VIMap = Map.Make(struct
    type t = variableIdentifier
    let compare = compare_variableIdentifier

    let t_of_sexp = variableIdentifier_of_sexp
    let sexp_of_t = sexp_of_variableIdentifier  
  end)

let rec eval v = function
  | Var vi -> VIMap.find v vi
  | Top -> Some true
  | Bottom -> Some false
  | Not f -> Option.map ~f:not (eval v f)
  | Box _ -> None
  | Diamond _ -> None
  | Imply (f, g) -> 
    let a = eval v f
    and b = eval v g in
    Option.map2 ~f:(||) a b
  | Equiv (f, g) -> 
    let a = eval v f
    and b = eval v g in
    Option.map2 ~f:(=) a b
  | And (f, g) -> 
    let a = eval v f
    and b = eval v g in
    Option.map2 ~f:(&&) a b
  | Or (f, g) -> 
    let a = eval v f
    and b = eval v g in
    Option.map2 ~f:(||) a b

let rec update = function
  | (i, true) :: x -> (i, false) :: update x
  | (i, false) :: x -> (i, true) :: x
  | [] -> []

let rec updatex ?(ret=[]) l k =
  if List.length ret >= k then
    ret
  else
    updatex ~ret:([l] @ List.map ~f:update ret) l k

let isTautology' f m = eval (VIMap.of_alist_exn m) f

let rec isTautology f =
  let ps = VISet.elements (getPropositionalLetters f) in
  let n = List.length ps in
  let l = List.map ~f:(fun s -> (s, false)) ps in
  let ls = updatex l (2 lsl (n - 1)) in
  List.fold ~init:(Some true) ~f:(Option.map2 ~f:(&&)) @@ List.map ~f:(isTautology' f) ls