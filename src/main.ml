open Command
open Formula
open Core

let input_str () = 
  Out_channel.(flush stdout);
  In_channel.(input_line_exn stdin)

let rec interactive env =
  Printf.printf ">> ";
  Out_channel.flush stdout;
  let cstr = input_str () in
  match (cparse cstr) with
  | Some Q -> Printf.printf "Quit.\n"; Out_channel.flush stdout
  | Some (Comment _) -> interactive env
  | Some c -> 
    Out_channel.flush stdout;
    (match (run c env) with
     | Some (Proved (thms, k)) ->
       let env = Proved (thms, k) in
       let str = "#" ^ (List.length thms - 1 |> string_of_int) ^ " |- " ^ (List.hd_exn thms |> show_formula) in
       Printf.printf "%s\n" str;
       Out_channel.flush stdout;
       interactive env
     | None ->  
       Printf.printf "Error\n";
       Out_channel.flush stdout;
       interactive env)
  | None ->
    Printf.printf "Error\n";
    Out_channel.flush stdout;
    interactive env

let _ =
  interactive @@ emptyEnv ()