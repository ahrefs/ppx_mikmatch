open Regexp_types

let parse_exn ~target ?(pos = Lexing.dummy_pos) s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- pos;
  lexbuf.lex_start_p <- pos;
  lexbuf.lex_abs_pos <- pos.pos_cnum;
  let mk_loc ?loc pos lexbuf =
    let open Lexing in
    let open Location in
    match loc with
    | Some loc -> loc
    | None ->
      (* no location from parser, use lexbuf positions *)
      {
        loc_ghost = false;
        loc_start = { pos with pos_cnum = pos.pos_cnum + lexbuf.lex_start_p.pos_cnum };
        loc_end = { pos with pos_cnum = pos.pos_cnum + lexbuf.lex_curr_p.pos_cnum };
      }
  in
  let main = match target with `Match -> Mik_parser.main_match_case | `Let -> Mik_parser.main_let_expr in
  try main Mik_lexer.token lexbuf with
  | Mik_lexer.Error msg ->
    let loc = mk_loc pos lexbuf in
    Location.raise_errorf ~loc "%s" msg
  | PError (loc, msg) ->
    let loc = mk_loc ~loc pos lexbuf in
    Location.raise_errorf ~loc "Syntax error: %s" msg
  | Mik_parser.Error ->
    let loc = mk_loc pos lexbuf in
    Location.raise_errorf ~loc "Syntax error"
