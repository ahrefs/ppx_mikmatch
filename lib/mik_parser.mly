%{
open Regexp_types

let nonepsilon = function { Location.txt = Seq []; _ } -> false | _ -> true

let mkloc = Location.mkloc

let simplify_seq ~loc es =
  match List.filter nonepsilon es with
  | [ e ] -> e
  | es -> mkloc (Seq es) loc

let simplify_alt es =
  match es with
  | [ e ] -> e.Location.txt
  | es -> Alt es

let make_loc startpos endpos =
  Location.{
    loc_start = startpos;
    loc_end = endpos;
    loc_ghost = false;
  }

let wrap_loc startpos endpos x =
  Location.{
    txt = x;
    loc = make_loc startpos endpos;
  }

let to_pcre_regex str startpos endpos =
  wrap_loc startpos endpos (Code str)

let syntax_error msg startpos endpos =
  let loc = make_loc startpos endpos in
  raise @@ PError (loc, msg)

let missing_error what startpos endpos =
  syntax_error (Printf.sprintf "Missing %s" what) startpos endpos

let unclosed_error what startpos endpos =
  syntax_error (Printf.sprintf "Unclosed %s" what) startpos endpos

let string_to_longident s =
  let open Ppxlib_ast.Ast in
  match String.split_on_char '.' s with
  | [] -> invalid_arg "string_to_longident: empty string"
  | [ x ] -> Lident x
  | x :: xs -> List.fold_left (fun acc s -> Ldot (acc, s)) (Lident x) xs

let last_component s =
  match List.rev (String.split_on_char '.' s) with
  | [] -> s
  | hd :: _ -> hd

let parse_flags s startpos endpos =
  let rec loop i flags =
    if i >= String.length s then flags
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> loop (i + 1) flags
      | 'i' -> loop (i + 1) { flags with case_insensitive = true }
      | 'u' -> loop (i + 1) { flags with anchored = false }
      | c -> syntax_error (Printf.sprintf "Unknown flag '%c'" c) startpos endpos
  in
  loop 0 mikmatch_default_flags
%}

%token <string> CHAR_LITERAL STRING_LITERAL IDENT MOD_IDENT PREDEFINED_CLASS INT
%token SLASH LPAREN RPAREN LBRACKET RBRACKET CARET LBRACE RBRACE EMPTY_STR
%token DASH BAR STAR PLUS QUESTION UNDERSCORE COLON EQUAL AS PIPE TILDE
%token INT_CONVERTER FLOAT_CONVERTER EOF

%start <string t * flags> main_match_case
%start <string t * flags> main_let_expr
%start <string t> pattern

/* operator precedence from lowest to highest */
%right AS                             /* priority 3: capture */
%left BAR                             /* priority 2: alternation */
%left seq                             /* priority 1: sequence (implicit) */
%nonassoc STAR PLUS QUESTION LBRACE   /* priority 0: repetition */

%%

main_match_case:
  | p = pattern EOF {
      p, mikmatch_default_flags
    }
  | SLASH p = pattern SLASH EOF {
      p, mikmatch_default_flags
    }
  | SLASH p = pattern SLASH flags = IDENT EOF {
      p, parse_flags flags $startpos(flags) $endpos(flags)
    }
  | SLASH pattern EOF { unclosed_error "pattern (missing closing '/')" $startpos($1) $endpos }
  | SLASH error { syntax_error "Invalid pattern after opening slash" $startpos($2) $endpos($2) }
  | error { syntax_error "Expected pattern to start with '/'" $startpos($1) $endpos($1) }

main_let_expr:
  | p = pattern EOF { p, mikmatch_default_flags }

pattern:
  | alt_expr { $1 }
  | alt_expr PIPE func = ident AS name = IDENT {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let func = string_to_longident func in
      wrap_loc $startpos $endpos (Pipe_all (name_loc, func, $1))
    }
  | alt_expr PIPE { missing_error "function name after '>>>'" $startpos($2) $endpos }
  | alt_expr PIPE ident { missing_error "'as' and result name after function" $startpos($3) $endpos }
  | alt_expr PIPE ident AS { missing_error "result name after 'as'" $startpos($4) $endpos }
  | { missing_error "pattern expression" $startpos $endpos }

alt_expr:
  | seq_expr { $1 }
  | seq_expr BAR alt_expr {
      let loc = make_loc $startpos $endpos in
      mkloc (simplify_alt [$1; $3]) loc
    }
  | seq_expr BAR { missing_error "expression after '|'" $startpos($2) $endpos }
  | BAR { missing_error "expression before '|'" $startpos $endpos }

seq_expr:
  | atom_expr { $1 }
  | atom_expr seq_expr %prec seq {
      let loc = make_loc $startpos $endpos in
      simplify_seq ~loc [$1; $2]
    }

atom_expr:
  | basic_atom { $1 }
  | basic_atom STAR {
      let repeat_loc = wrap_loc $startpos($2) $endpos($2) (0, None) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom PLUS {
      let repeat_loc = wrap_loc $startpos($2) $endpos($2) (1, None) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom QUESTION {
      wrap_loc $startpos $endpos (Opt $1)
    }
  | basic_atom TILDE {
      wrap_loc $startpos $endpos (Caseless $1)
    }
  | basic_atom LBRACE n = INT RBRACE {
      let n = int_of_string n in
      let repeat_loc = wrap_loc $startpos($2) $endpos($4) (n, Some n) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom LBRACE min = INT DASH max = INT RBRACE {
      let repeat_loc = wrap_loc $startpos($2) $endpos($6) (int_of_string min, Some (int_of_string max)) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom LBRACE min = INT DASH RBRACE {
      let repeat_loc = wrap_loc $startpos($2) $endpos($5) (int_of_string min, None) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  (* error cases for repetition *)
  | basic_atom LBRACE RBRACE { missing_error "repetition count" $startpos($2) $endpos($3) }
  | basic_atom LBRACE INT DASH DASH { syntax_error "Invalid repetition range" $startpos($4) $endpos($5) }
  | basic_atom LBRACE INT { unclosed_error "repetition (missing '}')" $startpos($2) $endpos }
  | basic_atom LBRACE error { syntax_error "Invalid repetition syntax" $startpos($2) $endpos }
  (* error cases for missing atoms before operators *)
  | STAR { missing_error "expression before '*'" $startpos $endpos }
  | PLUS { missing_error "expression before '+'" $startpos $endpos }
  | QUESTION { missing_error "expression before '?'" $startpos $endpos }

basic_atom:
  | CHAR_LITERAL {
      to_pcre_regex $1 $startpos $endpos
    }
  | STRING_LITERAL {
      to_pcre_regex $1 $startpos $endpos
    }
  | EMPTY_STR {
      to_pcre_regex "" $startpos $endpos
    }
  | UNDERSCORE {
      to_pcre_regex "." $startpos $endpos
    }
  | CARET {
      to_pcre_regex "^" $startpos $endpos
    }
  | PREDEFINED_CLASS {
      to_pcre_regex $1 $startpos $endpos
    }
  | IDENT | MOD_IDENT {
      let ident_loc = wrap_loc $startpos $endpos (string_to_longident $1) in
      wrap_loc $startpos $endpos (Call ident_loc)
    }

  | LBRACKET char_set RBRACKET {
      let set_str = "[" ^ $2 ^ "]" in
      to_pcre_regex set_str $startpos $endpos
    }
  | LBRACKET CARET char_set RBRACKET {
      let set_str = "[^" ^ $3 ^ "]" in
      to_pcre_regex set_str $startpos $endpos
    }
  | LBRACKET RBRACKET { missing_error "character set content" $startpos $endpos }
  | LBRACKET CARET RBRACKET { missing_error "character set content after '^'" $startpos $endpos }
  | LBRACKET char_set { unclosed_error "character set (missing ']')" $startpos($1) $endpos }
  | LBRACKET error { syntax_error "Invalid character set" $startpos $endpos }

  (* VARIABLE SUBSTITUTION *)

  (* Simple identifier captures, no name capture *)
  | LPAREN id = IDENT RPAREN
  | LPAREN id = MOD_IDENT RPAREN {
      (* (word) -> captures the result of calling 'word' pattern *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(id) $endpos(id) (last_component id) in
      wrap_loc $startpos $endpos (Capture_as (name_loc, None, call_node))
    }

  (* Simple captures with type conversion + function app *)
  | LPAREN id = IDENT COLON INT_CONVERTER RPAREN
  | LPAREN id = MOD_IDENT COLON INT_CONVERTER RPAREN {
      (* (digits : int) -> captures 'digits' pattern as 'digits' converted to int *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(id) $endpos(id) id in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Int, call_node))
    }
  | LPAREN id = IDENT COLON FLOAT_CONVERTER RPAREN
  | LPAREN id = MOD_IDENT COLON FLOAT_CONVERTER RPAREN {
      (* (number : float) -> captures 'number' pattern as 'number' converted to float *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(id) $endpos(id) id in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Float, call_node))
    }
  | LPAREN id = IDENT COLON typ = ident RPAREN
  | LPAREN id = MOD_IDENT COLON typ = ident RPAREN {
      (* (text : typ) -> captures 'text' pattern converted to 'typ' given that there is a parse_<typ> function in scope *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(id) $endpos(id) id in
      let typ = string_to_longident typ in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Typ typ), call_node))
    }
  | LPAREN id = IDENT COLON EQUAL func = ident RPAREN
  | LPAREN id = MOD_IDENT COLON EQUAL func = ident RPAREN {
      (* (text := process) -> captures 'text' pattern as 'text' processed by function *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(id) $endpos(id) id in
      let func = string_to_longident func in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func (func, None)), call_node))
    }
  | LPAREN id = IDENT COLON EQUAL func = ident COLON typ = ident RPAREN
  | LPAREN id = MOD_IDENT COLON EQUAL func = ident COLON typ = ident RPAREN {
      (* (text := process : typ) -> captures 'text' pattern as 'text' processed by function, with result as type 'typ' *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(id) $endpos(id) id in
      let func = string_to_longident func in
      let typ = string_to_longident typ in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func (func, Some typ)), call_node))
    }

  (* Simple named captures *)
  | LPAREN IDENT AS RPAREN | LPAREN MOD_IDENT AS RPAREN { missing_error "name after 'as'" $startpos($3) $endpos($4) }
  | LPAREN id = IDENT AS name = IDENT RPAREN
  | LPAREN id = MOD_IDENT AS name = IDENT RPAREN {
      (* (word as w) -> captures the result of calling 'word' as 'w' *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, None, call_node))
    }

  (* Named captures with type conversion + function app *)
  | LPAREN IDENT AS IDENT COLON RPAREN
  | LPAREN IDENT AS MOD_IDENT COLON RPAREN
  | LPAREN MOD_IDENT AS IDENT COLON RPAREN
  | LPAREN MOD_IDENT AS MOD_IDENT COLON RPAREN {
      missing_error "type converter after ':'" $startpos($5) $endpos($6) 
    }

  | LPAREN id = IDENT AS name = IDENT COLON INT_CONVERTER RPAREN
  | LPAREN id = MOD_IDENT AS name = IDENT COLON INT_CONVERTER RPAREN {
      (* (digits as n : int) -> captures 'digits' pattern as 'n' converted to int *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Int, call_node))
    }
  | LPAREN id = IDENT AS name = IDENT COLON FLOAT_CONVERTER RPAREN
  | LPAREN id = MOD_IDENT AS name = IDENT COLON FLOAT_CONVERTER RPAREN {
      (* (number as f : float) -> captures 'number' pattern as 'f' converted to float *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Float, call_node))
    }
  | LPAREN id = IDENT AS name = IDENT COLON typ = ident RPAREN
  | LPAREN id = MOD_IDENT AS name = IDENT COLON typ = ident RPAREN {
      (* (text : typ) -> captures 'text' pattern converted to 'typ' given that there is a parse_<typ> function in scope *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let typ = string_to_longident typ in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Typ typ), call_node))
    }
  | LPAREN id = IDENT AS name = IDENT COLON EQUAL func = ident RPAREN
  | LPAREN id = MOD_IDENT AS name = IDENT COLON EQUAL func = ident RPAREN {
      (* (text as t := process) -> captures 'text' pattern as 't' processed by function *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let func = string_to_longident func in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func (func, None)), call_node))
    }
  | LPAREN id = IDENT AS name = IDENT COLON EQUAL func = ident COLON typ = ident RPAREN
  | LPAREN id = MOD_IDENT AS name = IDENT COLON EQUAL func = ident COLON typ = ident RPAREN {
      (* (text as t := process : typ) -> captures 'text' pattern as 't' processed by function, with result as type 'typ' *)
      let call_loc = wrap_loc $startpos(id) $endpos(id) (string_to_longident id) in
      let call_node = wrap_loc $startpos(id) $endpos(id) (Call call_loc) in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let func = string_to_longident func in
      let typ = string_to_longident typ in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func (func, Some typ)), call_node))
    }

  (* Unclosed parentheses error cases for identifiers *)
  | LPAREN IDENT AS IDENT EOF?
  | LPAREN MOD_IDENT AS IDENT EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($4)
    }
  | LPAREN IDENT AS IDENT COLON INT_CONVERTER EOF?
  | LPAREN MOD_IDENT AS IDENT COLON INT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }
  | LPAREN IDENT AS IDENT COLON FLOAT_CONVERTER EOF?
  | LPAREN MOD_IDENT AS IDENT COLON FLOAT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }

  (* GENERAL PATTERNS *)

  (* Simple, no capture *)
  | LPAREN pattern RPAREN {
      $2
    }
  | LPAREN RPAREN { missing_error "pattern inside parentheses" $startpos $endpos }
  | LPAREN pattern EOF? { unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($2) }

  (* Simple named capture *)
  | LPAREN pattern AS RPAREN { missing_error "capture name after 'as'" $startpos($3) $endpos($4) }
  | LPAREN pattern AS COLON {
      missing_error "capture name between 'as' and ':'" $startpos($3) $endpos($4)
    }
  | LPAREN pattern AS COLON INT_CONVERTER RPAREN {
      missing_error "capture name between 'as' and ':'" $startpos($3) $endpos($4)
    }
  | LPAREN pattern AS COLON FLOAT_CONVERTER RPAREN {
      missing_error "capture name between 'as' and ':'" $startpos($3) $endpos($4)
    }
  | LPAREN pattern AS COLON EQUAL ident RPAREN {
      missing_error "capture name between 'as' and ':='" $startpos($3) $endpos($5)
    }
  | LPAREN pattern AS COLON EQUAL ident COLON ident RPAREN {
      missing_error "capture name between 'as' and ':='" $startpos($3) $endpos($5)
    }

  | LPAREN pattern AS name = IDENT RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, None, $2))
    }

  (* Named capture with type conversion + function app *)
  | LPAREN pattern AS IDENT COLON RPAREN {
      missing_error "type converter after ':'" $startpos($5) $endpos($6)
    }
  | LPAREN pattern AS IDENT COLON EQUAL RPAREN {
      missing_error "function name after ':='" $startpos($5) $endpos($6)
    }

  | LPAREN pattern AS name = IDENT COLON INT_CONVERTER RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Int, $2))
    }
  | LPAREN pattern AS name = IDENT COLON FLOAT_CONVERTER RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Float, $2))
    }
  | LPAREN pattern AS name = IDENT COLON typ = ident RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let typ = string_to_longident typ in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Typ typ), $2))
    }
  | LPAREN pattern AS name = IDENT COLON EQUAL func = ident RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let func = string_to_longident func in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func (func, None)), $2))
    }
  | LPAREN pattern AS name = IDENT COLON EQUAL func = ident COLON typ = ident RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let func = string_to_longident func in
      let typ = string_to_longident typ in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func (func, Some typ)), $2))
    }

  | LPAREN pattern AS IDENT EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($4)
    }
  | LPAREN pattern AS IDENT COLON INT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }
  | LPAREN pattern AS IDENT COLON FLOAT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }

  (* Generic error case *)
  | LPAREN error { syntax_error "Invalid expression in parentheses" $startpos($2) $endpos }

ident:
  | IDENT { $1 }
  | MOD_IDENT { $1 }

char_set:
  | char_set_item { $1 }
  | char_set_item char_set { $1 ^ $2 }

char_set_item:
  | CHAR_LITERAL { $1 }
  | CHAR_LITERAL DASH CHAR_LITERAL { $1 ^ "-" ^ $3 }
  | CHAR_LITERAL DASH { missing_error "character after '-' in range" $startpos($2) $endpos }
  | STRING_LITERAL { $1 }
  | INT { $1 }
  | PREDEFINED_CLASS { $1 }
  | IDENT { $1 }
  | IDENT DASH IDENT {
      if String.length $1 = 1 && String.length $3 = 1 then
        $1 ^ "-" ^ $3
      else
        syntax_error "ranges must be single characters" $startpos($2) $endpos
    }
