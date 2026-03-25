let%mikmatch username = {| alnum+ ('.' alnum+)* |}

type level = {%mikmatch| ("debug" | "info" | "warn" | "error" as level) |}

module M = struct
  let%mikmatch hex_lower = {| ['0'-'9' 'a'-'f'] |}

  module N = struct
    let%mikmatch hex = {| ['0'-'9' 'a'-'f' 'A'-'Z'] |}
    let something_else = "something else"
  end

  let something_else = "something else"
end

let () = match%mikmatch "ab12" with {|/ M.hex_lower+ /|} -> () | _ -> ()
let () = match%mikmatch "aB12" with {|/ M.N.hex+ /|} -> () | _ -> ()
let () = if M.something_else = "something else" then ()
let () = if M.N.something_else = "something else" then ()
