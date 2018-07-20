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

let runAsinteractiveMode () =
  interactive @@ emptyEnv ()

let runWithFilename ?(verbose=false) filename = 
  In_channel.with_file filename ~f:(fun ic ->
      let rec doNext line env =
        let s = In_channel.input_line ic in
        match s with
        | Some cstr -> (
            if verbose then
              Printf.printf "[line: %d] : %s\n" line cstr;
            match (cparse cstr) with
            | Some Q ->
              Printf.printf "Quit.\n";
              Out_channel.flush stdout
            | Some (Comment _) -> doNext (line + 1) env
            | Some c -> 
              Out_channel.flush stdout;
              (match (run c env) with
               | Some (Proved (thms, k)) ->
                 let env = Proved (thms, k) in
                 let str = "#" ^ (List.length thms - 1 |> string_of_int) ^ " |- " ^ (List.hd_exn thms |> show_formula) in
                 Printf.printf "%s\n" str;
                 Out_channel.flush stdout;
                 doNext (line + 1) env
               | None ->  
                 Printf.printf "Error at line : %d\n" line;
                 Out_channel.flush stdout)
            | None ->
              Printf.printf "Error at line : %d\n" line;
              Out_channel.flush stdout)
        | None -> 
          if line > 1 then
            Printf.printf "Ok\n"
          else
            Printf.printf "File is empty\n";
          Out_channel.flush stdout;
          ()
      in
      doNext 1 (emptyEnv ()))

let command =
  Command.basic
    ~summary:"Proof Assistant for Modal Logic in OCaml"
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: file)) in
      match filename with
      | Some fn -> fun () -> runWithFilename fn
      | None -> runAsinteractiveMode)

let _ = Command.run command