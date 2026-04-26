Made possible by forking [ppx_regexp](https://github.com/paurkedal/ppx_regexp).  
Our upstream contributions to `ppx_regexp` come from [another repo](https://github.com/ahrefs/ppx_regexp).

# PPX for Working with Regular Expressions

This repo provides a PPX providing regular expression-based routing:

`ppx_mikmatch` maps to [re][] with the conventional last-match extraction into `string` and `string option`.

This syntax extension turns:
```ocaml
function%mikmatch
| {| re1 |} -> e1
...
| {| reN |} -> eN
| _ -> e0
```
into suitable invocations of the [Re library][re], and similar for `match%mikmatch`.

It also accepts:
```ocaml
let%mikmatch var = {| some regex |}
```
to define reusable patterns, and much more.

## Full usage guide

[ppx_mikmatch guide](./MIKMATCH.md).

#### Quick Links
- [Variable capture](./MIKMATCH.md#variable-capture)
- [Type conversion](./MIKMATCH.md#type-conversion)
- [Different extensions](./MIKMATCH.md#alternatives)

## Small Example

```ocaml
(* Match HTTP method and path *)
let handle_request = function%mikmatch
  | {| "GET" ' '+ '/' (alpha+ as path) |} -> get_handler path
  | {| "POST" ' '+ '/' (alpha+ as path) |} -> post_handler path
  | _ -> method_not_allowed

(* Capture and convert to int *)
let parse_id = function%mikmatch
  | {| "id=" (digit+ as id : int) |} -> Some id
  | _ -> None
```

## Notice: Patterns are anchored by default

Regexes are anchored at both start and end by default. They can be unanchored with a flag.

```ocaml
(* This matches ONLY "hello", not "hello world" *)
function%mikmatch {| "hello" |} -> "matched"

(* Use /u flag for unanchored matching *)
function%mikmatch {|/ "hello" / u|} -> "matched" (* matches "hello world" *)
```

## Built-ins

Available in patterns:

**POSIX character classes:** `lower`, `upper`, `alpha`, `digit`, `alnum`, `punct`, `graph`, `print`, `blank`, `space`, `cntrl`, `xdigit` (7-bit ASCII, i.e. POSIX locale)

**Control sequences:**
- `bos` - beginning of string (`^`)
- `eos` - end of string (`$`)
- `bol` - beginning of line (start of string or after newline)
- `eol` - end of line (end of string or newline)
- `bnd` - word boundary (`\b`)
- `notnl` - any character except newline
- `any` - any character (including newline)

**Operators**:
  - **Repetition:** `*`, `+`, `?`, `{n}`, `{n-m}`, `{n-}`
  - **Caseless matching**: `~`

**Capturing** (`p` is either a pattern or an identifier for one):
  - **Unnamed**: `(p)`
  - **Named**: `(p as name)`
  - **Casting**: `(p as name : type)`
    - where `type` can be `int`, `float`, or any arbitrary type `<type>` with a parsing function `parse_<type>` available in scope
  - **Piping**: `(p as name := func)`
    - pipes match of `p` into `func` and names the result `name`

**Flags:** `i` (case-insensitive), `u` (unanchored)

## Motivational Examples

URL parsing:
```ocaml
let parse s =
  let (scheme, first) =
    match s.[4] with
    | ':' -> `Http, 7
    | 's' -> `Https, 8
    | _ -> failwith "parse"
  in
  let last = String.index_from s first '/' in
  let host = String.slice s ~first ~last in
  let (host,port) =
    match Stre.splitc host ':' with
    | exception _ -> host, default_port scheme
    | (host,port) -> host, int_of_string port
  in
  ...

(* in mikmatch: *)

let parse s =
  match%mikmatch s with
  | {|/ "http" ('s' as https)? "://" ([^ '/' ':']+ as host) (":" (digit+ as port : int))? '/'? (_* as rest) /|} ->
      let scheme = match https with Some _ -> `Https | None -> `Http in
      let port = match port with Some p -> p | None -> default_port scheme in
      ...
  | _ -> failwith "parse"

```

```ocaml
let rex =
  let origins = "csv|pdf|html|xlsv|xml"
  Re2.create_exn (sprintf {|^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z)(?:\.(\d+))?\.(%s)\.(\d+)\.(\d+)$|} origins)

let of_string s =
  try
    let m = Re2.first_match_exn rex s in
    let start = Re2.Match.get_exn ~sub:(`Index 1) m |> U.strptime "%Y-%m-%dT%H:%M:%S%z" |> U.timegm in
    let shard = int_of_string (Re2.Match.get_exn ~sub:(`Index 2) m) in
    let origin = origin_of_string (Re2.Match.get_exn ~sub:(`Index 3) m) in
    let partition = int_of_string (Re2.Match.get_exn ~sub:(`Index 4) m) in
    let worker = int_of_string (Re2.Match.get_exn ~sub:(`Index 5) m) in
    { start; shard; origin; partition; worker }
  with _ -> invalid_arg (sprintf "error: %s" s)

(* in mikmatch: *)

let%mikmatch origins = {| "csv" | "pdf" | "html" | "xlsv" | "xml" |}

let of_string s =
  match%mikmatch s with
  | {|/ (digit{4} '-' digit{2} '-' digit{2} 'T' digit{2} ':' digit{2} ':' digit{2} 'Z' as timestamp)
      ('.' (digit+ as shard : int))? 
      '.' (origins as origin := origin_of_string)
      '.' (digit+ as partition : int)
      '.' (digit+ as worker : int) /|} ->
      let start = U.strptime "%Y-%m-%dT%H:%M:%S%z" timestamp |> U.timegm in
      let shard = match shard with Some s -> s | None -> 0 in
      { start; shard; origin; partition; worker }
  | _ -> invalid_arg (sprintf "error: %s" s)

```

## Limitations

### No Exhaustiveness Check

The syntax extension will always warn if no catch-all case is provided.  No
exhaustiveness check is attempted.  Doing it right would require
reimplementing full regular expression parsing and an algorithm which would
ideally produce a counter-example.

## Bug Reports

The processor is currently new and not well tested.  Please break it and
file bug reports in the GitHub issue tracker.  Any exception raised by
generated code except for `Match_failure` is a bug.

[re]: https://github.com/ocaml/ocaml-re
