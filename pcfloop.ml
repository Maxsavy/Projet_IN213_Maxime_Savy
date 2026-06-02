let version = "0.01" ;;

let print_value oc = function
  | Lambda.LAbs (x, Lambda.LAbs (_, Lambda.LVar x')) when x = x' ->
      Printf.fprintf oc "true"
  | Lambda.LAbs (_, Lambda.LAbs (y, Lambda.LVar y')) when y = y' ->
      Printf.fprintf oc "false"
  | t -> Lambda.print_term oc t

let load_lib path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  let ast = Pcfparse.main Pcflex.lex lexbuf in
  close_in ic;
  ast
;;

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\tRead a PCF program from file (default is stdin)\n%!"
    Sys.argv.(0) in
  exit 1
;;

let main() =
  let lib_ast = load_lib "lib.mml" in
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 -> (
        match Sys.argv.(1) with
        | "-" -> stdin
        | name ->
            (try open_in name with
            |_ -> Printf.eprintf "Opening %s failed\n%!" name; exit 1)
       )
    | n -> usage () in
  let _ = Printf.printf "        Welcome to PCF, version %s\n%!" version in
  let lexbuf = Lexing.from_channel input_channel in
  while true do
    try
      let _ = Printf.printf  "> %!" in
      let e = Pcfparse.main Pcflex.lex lexbuf in
      let combined = Pcfast.wrap_with_lib lib_ast e in
      let _ = Printf.printf "Recognized: " in
      let _ = Pcfast.print stdout combined in
      let _ = Printf.fprintf stdout " =\n%!" in
      let my_lambda = Translate.translate_expr combined in
      let _ = Printf.printf "λ-term:  " in
      let _ = Lambda.print_term stdout my_lambda in
      let _ = Printf.printf "\nresult:  " in
      let _ = print_value stdout (Lambda.normalize my_lambda) in
      Printf.printf "\n\n%!"
    with
    | Pcflex.Eoi -> Printf.printf  "Bye bye.\n%!" ; exit 0
    | Failure msg -> Printf.printf "Erreur: %s\n\n" msg
    | Parsing.Parse_error ->
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        Format.printf
          "File %S, line %i, characters %i-%i: Syntax error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
    | Pcflex.LexError (sp, ep) ->
        Printf.printf
          "File %S, line %i, characters %i-%i: Lexical error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
  done
;;

if !Sys.interactive then () else main () ;;
