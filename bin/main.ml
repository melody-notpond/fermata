open Fermata

let file =
  if Array.length Sys.argv != 2 then begin
    Printf.eprintf "usage: %s [file]" Sys.argv.(0);
    exit 1
  end else
    let name = Sys.argv.(1) in
    In_channel.open_text name

let p = parse_channel file
let () = In_channel.close file

let () = print_endline @@ Ast.string_of_prog p
