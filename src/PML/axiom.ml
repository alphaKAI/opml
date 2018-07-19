open Formula
open Core

(* [](p->q)->([]p->[]q) *)
let axiomK = Imply ((Box (Imply ((Var (VariableIdentifier "p")), (Var (VariableIdentifier "q"))))), 
                    (Imply ((Box (Var (VariableIdentifier "p"))), (Box (Var (VariableIdentifier "q"))))))

(* <>p<->~[]~p *)
let axiomDualD = Equiv ((Diamond (Var (VariableIdentifier "p"))), (Not (Box (Not (Var (VariableIdentifier "p"))))))


(* []p<->~<>~p *)
let axiomDualB = Equiv ((Box (Var (VariableIdentifier "p"))), (Not (Diamond (Not (Var (VariableIdentifier "p"))))))

let axiom = [axiomK; axiomDualB; axiomDualD]

let isAxiom g =
  match isTautology g with
  | None -> (
      List.find axiom ~f:(fun k -> k = g)
      |> function
      | Some _ -> true
      | _ -> false)
  | Some x -> x