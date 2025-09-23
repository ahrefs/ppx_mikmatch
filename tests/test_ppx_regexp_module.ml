let%mikmatch username = {| alnum+ ('.' alnum+)* |}

type level = {%mikmatch| ("debug" | "info" | "warn" | "error" as level) |}
