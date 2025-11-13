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

### Full usage guide

[ppx_mikmatch guide](./MIKMATCH.md).

#### Quick Links
- [Variable capture](./MIK.md#variable-capture)
- [Type conversion](./MIK.md#type-conversion)
- [Different extensions](./MIK.md#alternatives)

#### Motivational Examples

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

## Performance Considerations

The different syntax extensions behave differently:
  - `match%mikmatch` will compile all branches into suitable groups.
    The group creation follows the invariant:

    If the regexes in the current group:
    1. do not have pattern guards, then if the current regex:
        1. is pattern guardless as well, it can belong to the same group
        2. has a pattern guard, it starts a new group
    2. have pattern guards, then if the current regex
        1. has the same RE and flags, it can belong to the same group
        2. doesn't have the same RE and flags, then start new group

    Each group is compiled as a single Regex using alternations and tried against the input string.

  - the general extension defined [here](MIKMATCH.md#general-matchfunction) compiles each branch into a separate Regex, so it is less efficient than the first option.

When compared to `mikmatch` or `Re2` using `Match.get_exn`, `ppx_mikmatch` is considerably faster, as the other tools take the same approach as the general extension.

A comparison:
```ocaml
(* the REs used here are direct equivalents to the branches in the mikmatchlike functions below *)
let extract_httpheader_re2 s =
  match Re2.first_match content_encoding_re s with
  | Ok mtch ->
    let v = Re2.Match.get_exn ~sub:(`Index 1) mtch in
    `ContentEncoding (String.lowercase_ascii @@ strip v)
  | Error _ ->
  ...
  match Re2.first_match link_re s with
  | Ok mtch ->
    let url = Re2.Match.get_exn ~sub:(`Index 1) mtch in
    let rest = Re2.Match.get_exn ~sub:(`Index 2) mtch in
    `Link (url, String.lowercase_ascii @@ strip rest)
  | Error _ -> `Other
end

let extract_httpheader_mikmatch s =
  match s with
  | / "content-encoding:"~ ' '* (_* as v) "\r\n"? eos / -> `ContentEncoding (String.lowercase_ascii @@ strip v)
  | / "content-type:"~ ' '* (_* as v) "\r\n"? eos / -> `ContentType (String.lowercase_ascii @@ strip v)
  | / "last-modified:"~ ' '* (_* as v) "\r\n"? eos / -> `LastModified (strip v)
  | / "content-length:"~ ' '* (_* as v) "\r\n"? eos / -> `ContentLength (strip v)
  | / "etag:"~ ' '* (_* as v) "\r\n"? eos / -> `ETag (strip v)
  | / "server:"~ ' '* (_* as v) "\r\n"? eos / -> `Server (strip v)
  | / "x-robots-tag:"~ ' '* (_* as v) "\r\n"? eos / -> `XRobotsTag (strip v)
  | / "location:"~ ' '* (_* as v) "\r\n"? eos / -> `Location (strip v)
  | / "link:"~ ' '* '<' (re_link_url as url) '>' ' '* ';' (_* as rest) "\r\n"? eos / -> `Link (url, String.lowercase_ascii @@ strip rest)
  | _ -> "Other"

let extract_httpheader_ppx_mikmatch s =
  match%mikmatch s with
  | {| "content-encoding:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `ContentEncoding (String.lowercase_ascii v)
  | {| "content-type:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `ContentType (String.lowercase_ascii v)
  | {| "last-modified:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `LastModified v
  | {| "content-length:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `ContentLength v
  | {| "etag:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `ETag v
  | {| "server:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `Server v
  | {| "x-robots-tag:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `XRobotsTag v
  | {| "location:"~ ' '* (_* as v := String.strip) "\r\n"? |} -> `Location v
  | {| "link:"~ ' '* '<' (re_link_url as url) '>' ' '* ';' (_* as rest := String.strip) "\r\n"? |} ->
    `Link (url, String.lowercase_ascii rest)
  | _ -> "Other"
```

Benchmarking these three yields:
```bash
run_bench 3 cases (count 10000)
         re2 : allocated    496.4MB, heap         0B, collection 0 0 248, elapsed 1.01 seconds, 9887.97/sec : ok
    mikmatch : allocated    147.9MB, heap         0B, collection 0 0 73, elapsed 0.2817 seconds, 35504.18/sec : ok
ppx_mikmatch : allocated    155.5MB, heap         0B, collection 0 0 77, elapsed 0.0669 seconds, 149445.91/sec : ok
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
