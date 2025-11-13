
# `%mikmatch` extension

Accepts `mikmatch` syntax, along with some nice to haves.

## Grammar
The grammar accepted by this extensions is the following

```bnf
<main_match_case> ::= <pattern> EOF
                    | "/" <pattern> "/" <flags> EOF

<flags> ::= "" | "i" <flags> | "u" <flags>

<main_let_expr> ::= <pattern> EOF

<pattern> ::= <alt_expr>
            | <alt_expr> ">>>" <func_name> "as" IDENT

<alt_expr> ::= <seq_expr>
             | <seq_expr> "|" <alt_expr>

<seq_expr> ::= <atom_expr>
             | <atom_expr> <seq_expr>

<atom_expr> ::= <basic_atom>
              | <basic_atom> "*"
              | <basic_atom> "+"
              | <basic_atom> "?"
              | <basic_atom> "~"                         # caseless matching
              | <basic_atom> "{" INT (n) "}"             # match n times
              | <basic_atom> "{" INT (n) "-" INT (m) "}" # match at least n times, at most m times

<basic_atom> ::= CHAR_LITERAL
               | STRING_LITERAL
               | EMPTY_STR
               | "_"
               | "^"
               | PREDEFINED_CLASS
               | IDENT
               | "[" <char_set> "]"     # character class
               | "[" "^" <char_set> "]" # negative character class
               | "(" <pattern> ")"
               | "(" IDENT ")"
               | "(" IDENT "as" IDENT ")"
               | "(" IDENT "as" IDENT ":" "int" ")"
               | "(" IDENT "as" IDENT ":" "float" ")"
               | "(" IDENT "as" IDENT ":=" <func_name> ")"
               | "(" <pattern> "as" IDENT ")"
               | "(" <pattern> "as" IDENT ":" "int" ")"
               | "(" <pattern> "as" IDENT ":" "float" ")"
               | "(" <pattern> "as" IDENT ":=" <func_name> ")"

<func_name> ::= IDENT
              | MOD_IDENT # qualified names

<char_set> ::= <char_set_item>
             | <char_set_item> <char_set>

<char_set_item> ::= CHAR_LITERAL
                  | CHAR_LITERAL "-" CHAR_LITERAL
                  | STRING_LITERAL
                  | PREDEFINED_CLASS
                  | IDENT
```

Where `PREDEFINED_CLASS` is one of:
  - **POSIX character classes:** `lower`, `upper`, `alpha`, `digit`, `alnum`, `punct`, `graph`, `print`, `blank`, `space`, `cntrl`, `xdigit`
  - **Control sequences:** `eos` (same as `$`), `eol` (end of string or newline), `bnd` (word boundary `\b`), `bos` (same as `^`), `any` (any character except newline)
  - **Empty string:** `""`, equivalent to `^$` (or `bos eos`)

## Semantics and Examples
### Variable substitution
```ocaml
let%mikmatch re1 = {| "hello" |}
let%mikmatch re2 = {| re1 ' ' "world" |}

let do_something = function%mikmatch
  | {|/ ... (re2) ... /|} -> ...
  | _ -> ...

(* will expand to *)
let do_something = function%mikmatch
  | {|/ ... ("hello" ' ' "world") ... /|} -> ...
  | _ -> ...
```

### Variable capture
```ocaml
let%mikmatch num = {| digit+ |}

let do_something = function%mikmatch
  | {|/ ... (num as n) ... /|} -> ... (* (n : string) available here *)
  | _ -> ...
```

Values are also available at the guard level:

```ocaml
let%mikmatch num = {| digit+ |}

let do_something = function%mikmatch
  | {|/ ... (num as n) ... /|} when n = "123" -> ...
  | _ -> ...
```

#### Type conversion
It is possible to convert variables to `int`, `float`, or some other type (given there is a `parse` function for it in scope) on the fly:

```ocaml
let%mikmatch num = {| digit+ |}

let parse_custom_typ t = ...

let do_something = function%mikmatch
  | {|/ 'd' (num as n : int) ... /|} when n = 123 -> ... (* (n : int) available here *)
  | {|/ 'f' (num as n : float) ... /|} -> ... (* (n : float) available here *)
  | {|/ "other" (any+ as c : custom_typ) ... /|} -> ... (* (c : custom_typ) available here *)
  | _ -> ...
```

It is also possible to pass the variables into any `string -> 'a` function:
```ocaml
let%mikmatch ip = {| (digit{1-3} '.'){3} digit{1-3}|}
let parse_ip = String.split_on_char '.'
let get_ip = function%mikmatch
  | {|/ ... (ip as ip := parse_ip) ... /|} -> ... (* (ip : string list) available here *)
  | _ -> ...

let get_upper_name = function%mikmatch
  | {|/ ... (['a'-'z'] as name := String.uppercase) ... /|} -> ... (* (name : string) available here *)
  | _ -> ...
```

#### Piping to a catch all function

Using the `>>>` syntax extension, you can pipe all bound named variables into a single function, and name its return value.

```ocaml
type example = {
  name : string;
  num : int;
  mode : [ `A | `B | `Default ];
}

let mk_example name num mode = match mode with
  | Some 'a' -> { name; num; mode = `A}
  | Some 'b' -> { name; num; mode = `B}
  | Some _ | None -> { name; num; mode = `Default}

let mk_example_re = function%mikmatch
  | {|/ (['a'-'z']~ as name := String.capitalize_ascii) ' ' (digit+ as num : int) ' ' ('a'|'b' as mode)? >>> mk_example as res /|} -> (* (res : example) available here, and all other bound variables *)
  | _ -> ...
```

### Default catch-all case
The PPX generates a default catch-all case if none is provided. This catch-all case executes if none of the RE match cases does, and it raises a `Failure` exception with the location of the function and name of the file where it was raised.

## Flags

The `/` delimiters are optional, except if flags are needed using the syntax `/ ... / flags`, where `flags` can be
- `i` for caseless matching
- `u` for unanchored matching (`%mikmatch` is anchored at the beginning and end by default)
- or both

## Alternatives
### Defining variables
You have a choice between:
```ocaml
let%mikmatch re = {|some regex|}
(* and *)
let re = {%mikmatch|some regex|}
```


### Matching:
#### `match%mikmatch` and `function%mikmatch`

```ocaml
function%mikmatch
  | {|/ some regex /|} -> ...
  | {|/ another regex /|} -> ...
  ...
  | _ -> ...
```

This match expression will compile all of the REs in the branches into one, with some exceptions around pattern guards, and use marks to find which branch was executed.  
Efficient if you have multiple branches.

The regexes are anchored both at the beginning, and at the end. So, for example, the first match case will be compiled to `^some regex$`.

#### General match/function

```ocaml
function
  | "some string" -> ...
  | {%mikmatch|/ some regex /|} -> ...
  ...
  | "another string" -> ...
  | {%mikmatch|/ some regex /|} -> ...
  ...
  | _ -> ...
```

This match expression will compile all of the REs **individually**, and test each one in sequence.  
It keeps all of the features (guards and such) of the previous extension, explored in [Semantics](#Semantics_and_Examples)

### Type definitions from patterns
You can generate record types from regex patterns:

```ocaml
type url = {%mikmatch|
  (("http" | "https") as scheme) "://"
  ((alnum+ ('.' alnum+)*) as host)
  (':' (digit+ as port : int))?
  ('/' ([^'?' '#']* as path))?
  ('?' ([^'#']* as query))?
  ('#' (any* as fragment))?
|}
```
This generates:
- A record type with fields for each named capture
- `parse_url : string -> url option` - parses strings into the type
- `pp_url : Format.formatter -> url -> unit` - pretty-prints back to string format

> [!WARNING]
> When printing, repetitions will be executed the minimum required amount of times.  
> `*` prints nothing

#### Smart reconstruction
The pretty-printer intelligently handles alternations and optional fields:
```ocaml
type mode =
  [ `A
  | `B
  | `Other
  ]

let mk_mode = function "a" -> `A | "b" -> `B | _ -> `Other
let pp_mode fmt mode = Format.fprintf fmt @@ match mode with `A -> "a" | `B -> "b" | `Other -> "other"

let%mikmatch date_format = {| digit{4} '-' digit{2} '-' digit{2} ' ' digit{2} ':' digit{2} ':' digit{2} |}

type log = {%mikmatch|
  (date_format as date)
  " [" (upper+ as level) "]"
  ((" pid=" (digit+ as pid : int))? | (" name=" ([a-z]+ as name))?)
  ' '{2-3}
  ('a'|'b'|"other" as mode := mk_mode : mode)
  ": "
  (any+ as message)
|}

let input = "2025-06-13 12:42:12 [INFO] pid=123  a: something happened" in
match parse_log input with
| Some log ->
  (* Prints: "2025-06-13 12:42:12 [INFO] pid=123  a: something happened" *)
  Format.printf "%a@." pp_log log;
  
  (* Change from pid to name variant *)
  let log' = { log with pid = None; name = Some "server" } in
  (* Prints: "2025-06-13 12:42:12 [INFO] name=server  a: something happened" *)
  Format.printf "%a@." pp_log log'
```
The pretty-printer detects which alternation branch to use based on field population - if `pid` is `Some _`, it prints the `pid` branch; if `name` is `Some _`, it prints the `name` branch.

##### Type conversions and custom parsers
- For function application you are required to pass the return type.
- If the return type is itself an application (e.g. `string list`), then you must provide a type alias.  
- For function application with `:=`, the type must have an associated `pp` function. (Notice, in the example, the `mode` type and its associated functions)
- If the type is provided without a conversion function, then it is assumed that in the scope there are associated `parse` and `pp` functions.
  This guarantees compositionality with other types defined with this extension


