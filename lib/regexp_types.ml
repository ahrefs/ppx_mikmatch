exception PError of Location.t * string

let mkloc = Location.mkloc
let ( % ) f g x = f (g x)

type 'a t = 'a node Location.loc

and 'a node =
  | Code of 'a
  | Seq of 'a t list
  | Alt of 'a t list
  | Opt of 'a t
  | Repeat of (int * int option) Location.loc * 'a t
  | Nongreedy of 'a t
  | Caseless of 'a t
  | Capture of 'a t
  | Capture_as of string Location.loc * conv_ty option * 'a t
  | Pipe_all of string Location.loc * Longident.t * 'a t
  | Call of Longident.t Location.loc
(* TODO: | Case_blind of t *)

and conv_ty =
  | Int
  | Float
  | Typ of Longident.t
  | Func of (Longident.t * Longident.t option) (* function name * type *)
  | Pipe_all_func of Longident.t

type flags = {
  case_insensitive : bool; (* i *)
  anchored : bool; (* for pcre: a - true | for mikmatch: u - false*)
}

let pcre_default_flags = { case_insensitive = false; anchored = false }
let mikmatch_default_flags = { case_insensitive = false; anchored = true }
