open Ppxlib
open Ast_builder.Default

module Re_comp = struct
  let apply_re_flags ~loc re_expr flags =
    let open Regexp_types in
    let re = if flags.case_insensitive then [%expr Re.no_case [%e re_expr]] else re_expr in
    let re = if flags.anchored then [%expr Re.whole_string [%e re]] else re in
    re

  let compile_single ~loc re flags = [%expr Re.compile [%e apply_re_flags ~loc re flags]]

  let compile_group ~loc res_with_flags =
    let marked = List.map (fun (re, flags) -> [%expr Re.mark [%e apply_re_flags ~loc re flags]]) res_with_flags in
    [%expr
      let a = [%e pexp_array ~loc marked] in
      let marks = Array.map fst a in
      let re = Re.compile (Re.alt (Array.to_list (Array.map snd a))) in
      re, marks]

  let compile ~loc var_name re_array_with_flags =
    let comp_expr =
      match re_array_with_flags with
      | [ (re, flags) ] ->
        [%expr
          let re = [%e compile_single ~loc re flags] in
          re, ([||] : Re.Group.t array)]
      | _ -> compile_group ~loc re_array_with_flags
    in
    value_binding ~loc ~pat:(ppat_var ~loc { txt = var_name; loc }) ~expr:comp_expr
end

module Bindings = struct
  let wrap_group_expr ~loc ~mustG iG offG conv =
    let eG = match iG with None -> [%expr Re.Group.get _g 0] | Some iG -> [%expr Re.Group.get _g [%e eint ~loc (offG + iG + 1)]] in
    let eG =
      match conv with
      | None -> eG
      | Some Regexp_types.Int -> [%expr int_of_string [%e eG]]
      | Some Float -> [%expr float_of_string [%e eG]]
      | Some (Typ t) ->
        let parse_fun = Util.mk_qualified_fun ~prefix:"parse_" ~loc t in
        [%expr [%e parse_fun] [%e eG]]
      | Some (Func (func_name, _)) ->
        let func_ident = pexp_ident ~loc { txt = func_name; loc } in
        [%expr [%e func_ident] [%e eG]]
      | Some (Pipe_all_func _) -> Util.error ~loc ">>> not allowed inside patterns"
    in
    if mustG then eG else [%expr try Some [%e eG] with Not_found -> None]

  let rec wrap_group_bindings ~loc ~captured_acc rhs offG = function
    | [] -> rhs
    | [ (varG, _, Some (Regexp_types.Pipe_all_func func_name), _) ] ->
      let func_ident = pexp_ident ~loc { txt = func_name; loc } in
      let captured = List.rev captured_acc in
      let func_app = List.fold_left (fun expr arg -> [%expr [%e expr] [%e arg]]) func_ident captured in
      [%expr
        let [%p ppat_var ~loc varG] = [%e func_app] in
        [%e rhs]]
    | (varG, iG, conv, mustG) :: bs ->
      let eG = wrap_group_expr ~loc ~mustG iG offG conv in
      let pat = ppat_var ~loc varG in
      let pat_ident = pexp_ident ~loc @@ { txt = Util.extract_qualified_name varG.txt; loc = varG.loc } in
      [%expr
        let [%p pat] = [%e eG] in
        [%e wrap_group_bindings ~loc ~captured_acc:(pat_ident :: captured_acc) rhs offG bs]]
end

module Regexp = struct
  open Regexp_types
  include Regexp

  let bindings =
    let rec recurse must_match (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      match e'.Location.txt with
      | Code _ -> fun acc -> acc
      | Seq es -> Util.List.fold (recurse must_match) es
      | Alt es -> Util.List.fold (recurse false) es
      | Opt e -> recurse false e
      | Repeat ({ Location.txt = i, _; _ }, e) -> recurse (must_match && i > 0) e
      | Nongreedy e -> recurse must_match e
      | Caseless e -> recurse must_match e
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mikmatch."
      | Capture_as (idr, conv, e) -> fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Pipe_all (res, func, e) ->
        fun (nG, bs) ->
          let nG', inner_bs = recurse must_match e (nG, []) in
          nG', ((res, None, Some (Pipe_all_func func), must_match) :: inner_bs) @ bs
      | Call _ -> fun (nG, bs) -> nG + 1, bs
    in
    function
    | { Location.txt = Capture_as (idr, conv, e); _ } -> recurse true e (1, [ idr, Some 0, conv, true ])
    | e -> recurse true e (0, [])

  let to_re_expr ?(group = true) ~in_let =
    let rec recurse (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      match e'.Location.txt with
      | Code s -> [%expr Re.Perl.re [%e estring ~loc s]]
      | Seq es -> [%expr Re.seq [%e elist ~loc (List.map recurse es)]]
      | Alt es -> [%expr Re.alt [%e elist ~loc (List.map recurse es)]]
      | Opt e -> [%expr Re.opt [%e recurse e]]
      | Repeat ({ Location.txt = i, j_opt; _ }, e) ->
        let e_i = eint ~loc i in
        let e_j = match j_opt with None -> [%expr None] | Some j -> [%expr Some [%e eint ~loc j]] in
        [%expr Re.repn [%e recurse e] [%e e_i] [%e e_j]]
      | Nongreedy e -> [%expr Re.non_greedy [%e recurse e]]
      | Caseless e -> [%expr Re.no_case [%e recurse e]]
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mikmatch."
      | Capture_as (_, _, e) -> if group then [%expr Re.group [%e recurse e]] else recurse e
      | Pipe_all (_, _, e) -> recurse e
      | Call lid ->
        (* Use the Call node's own location, not the parent's location *)
        let ld = { txt = lid.txt; loc = lid.loc } in
        if in_let || not group then pexp_ident ~loc:lid.loc ld else [%expr Re.group [%e pexp_ident ~loc:lid.loc ld]]
    in
    function
    | { Location.txt = Capture_as (_, _, e); _ } ->
      let loc = e.Location.loc in
      if group then [%expr Re.group [%e recurse e]] else recurse e
    | e -> recurse e

  let rec squash_codes (e : _ Location.loc) : _ Location.loc =
    let open Location in
    let rec combine (nodes : _ Location.loc list) =
      match nodes with
      | [] -> []
      | { Location.txt = Code s1; loc = loc1 } :: { Location.txt = Code s2; loc = loc2 } :: rest ->
        let combined_loc =
          if loc1 = Location.none || loc2 = Location.none then Location.none
          else Location.{ loc_start = loc1.loc_start; loc_end = loc2.loc_end; loc_ghost = false }
        in
        combine ({ Location.txt = Code (s1 ^ s2); loc = combined_loc } :: rest)
      | node :: rest -> node :: combine rest
    in
    match e.txt with
    | Code _ -> e
    | Seq es ->
      let es = List.map squash_codes es in
      { e with txt = Seq (combine es) }
    | Alt es ->
      let es = List.map squash_codes es in
      { e with txt = Alt es }
    | Opt e' -> { e with txt = Opt (squash_codes e') }
    | Repeat (range, e') -> { e with txt = Repeat (range, squash_codes e') }
    | Nongreedy e' -> { e with txt = Nongreedy (squash_codes e') }
    | Caseless e' -> { e with txt = Caseless (squash_codes e') }
    | Capture e' -> { e with txt = Capture (squash_codes e') }
    | Capture_as (name, j, e') -> { e with txt = Capture_as (name, j, squash_codes e') }
    | Pipe_all (r, f, e') -> { e with txt = Pipe_all (r, f, squash_codes e') }
    | Call _ -> e

  let check_alternation_captures ~loc pattern =
    let rec get_alt_captures = function
      | { Location.txt = Regexp_types.Alt branches; _ } -> Some (List.map get_branch_captures branches)
      | { Location.txt = Seq es; _ } -> List.find_map get_alt_captures es
      | { Location.txt = Capture_as (_, _, e); _ } -> get_alt_captures e
      | _ -> None
    and get_branch_captures branch =
      let rec collect_captures acc = function
        | { Location.txt = Regexp_types.Capture_as (name, _, e); _ } -> collect_captures (name.txt :: acc) e
        | { Location.txt = Seq es; _ } -> List.fold_left collect_captures acc es
        | { Location.txt = Opt e; _ }
        | { Location.txt = Repeat (_, e); _ }
        | { Location.txt = Nongreedy e; _ }
        | { Location.txt = Caseless e; _ }
        | { Location.txt = Capture e; _ } ->
          collect_captures acc e
        | { Location.txt = Alt _; _ } -> acc
        | _ -> acc
      in
      collect_captures [] branch |> List.rev
    in

    match get_alt_captures pattern with
    | None -> fun expr -> expr
    | Some branches_captures ->
      (* check if different branches have different capture variables *)
      let all_vars = List.concat branches_captures |> List.sort_uniq String.compare in
      let branch_var_sets = List.map (List.sort_uniq String.compare) branches_captures in

      (* if not all branches have the same variables, issue warning *)
      let has_inconsistent_captures =
        List.exists (fun branch_vars -> List.length branch_vars <> List.length all_vars || branch_vars <> all_vars) branch_var_sets
      in

      if has_inconsistent_captures && List.length all_vars > 1 then begin
        let warning_msg =
          Printf.sprintf
            {| This let destruct pattern has alternations with different capture groups (%s).
               Only one branch will match at runtime, but all variables are being bound.
               Consider using a single capture group over the alternations. |}
            (String.concat ", " all_vars)
        in
        Util.warn ~loc warning_msg
      end
      else fun expr -> expr
end

module Parser = struct
  let calculate_pattern_pos ~loc ~pattern_str =
    let open Lexing in
    match loc with
    | { Location.loc_start; loc_end; _ } ->
      let total_len = loc_end.pos_cnum - loc_start.pos_cnum in
      let pattern_len = String.length pattern_str in

      let delimiter_overhead = total_len - pattern_len in
      let start_delimiter_size = delimiter_overhead / 2 in

      { loc_start with pos_cnum = loc_start.pos_cnum + start_delimiter_size }

  let get_parser ~target ~pos = Regexp.parse_exn ~target ~pos

  let run ~parser ~target ~pos:_ s =
    let r, flags = parser s in
    let r = Regexp.squash_codes r in
    let nG, bs = Regexp.bindings r in
    let re = Regexp.to_re_expr ~in_let:(target = `Let) r in
    r, re, bs, nG, flags
end

let make_default_rhs ~target ~loc = function
  | [] ->
    let open Lexing in
    let pos = loc.Location.loc_start in
    let pos_end = loc.Location.loc_end in

    let context = match target with `Match -> "any mikmatch cases" | `Let -> "the mikmatch regex" in

    let location_desc =
      let char_start = pos.pos_cnum - pos.pos_bol in
      let char_end = pos_end.pos_cnum - pos_end.pos_bol in
      if pos.pos_lnum = pos_end.pos_lnum then Printf.sprintf "line %d, characters %d-%d" pos.pos_lnum char_start char_end
      else Printf.sprintf "lines %d-%d, characters %d-%d" pos.pos_lnum pos_end.pos_lnum char_start char_end
    in

    let err_msg = Printf.sprintf "File %s, %s: String did not match %s." pos.pos_fname location_desc context in
    [%expr raise (Failure [%e estring ~loc err_msg])]
  | default_cases ->
    let transformed =
      List.map
        begin
          fun case ->
            match case.pc_lhs.ppat_desc with
            | Ppat_var var ->
              {
                case with
                pc_lhs = ppat_any ~loc;
                pc_rhs =
                  [%expr
                    let [%p ppat_var ~loc var] = _ppx_regexp_v in
                    [%e case.pc_rhs]];
              }
            | _ -> case
        end
        default_cases
    in
    begin
      match transformed with
      | [ { pc_lhs = { ppat_desc = Ppat_any; _ }; pc_guard = None; pc_rhs; _ } ] -> pc_rhs
      | _ -> pexp_match ~loc [%expr _ppx_regexp_v] transformed
    end

let build_exec_match ~loc ~re_var ~continue_next ~on_match =
  [%expr match Re.exec_opt (fst [%e re_var]) _ppx_regexp_v with None -> [%e continue_next] | Some _g -> [%e on_match]]

(* Transformations *)

let transform_let ~loc vb =
  let parser = Parser.get_parser ~target:`Let in
  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
  | Ppat_var { txt = _; _ }, Pexp_constant (Pconst_string (value, _, _)) ->
    let pos = loc.Location.loc_start in
    let parsed, _flags = parser ~pos value in
    let parsed = Regexp.squash_codes parsed in
    let re_expr = Regexp.to_re_expr ~in_let:true parsed in
    let expr = [%expr [%e re_expr]] in
    { vb with pvb_expr = expr }
  | _ -> vb

let transform_destructuring_let ~loc pattern_str expr =
  let target = `Match in
  let pos = Parser.calculate_pattern_pos ~loc ~pattern_str in
  let parser = Parser.get_parser ~target ~pos in
  let r, re, bs, _, flags = Parser.run ~parser ~target ~pos pattern_str in
  let capture_names = List.map (fun (name, _, _, _) -> name) (List.rev bs) in

  let lhs_pattern =
    match capture_names with
    | [] -> [%pat? ()]
    | [ name ] -> ppat_var ~loc name
    | names -> ppat_tuple ~loc (List.map (fun n -> ppat_var ~loc n) names)
  in

  let re_var = Util.fresh_var () in
  let re_binding = Re_comp.compile ~loc re_var [ re, flags ] in

  let on_match =
    let apply_conv ~loc expr = function
      | None -> expr
      | Some Regexp_types.Int -> [%expr int_of_string [%e expr]]
      | Some Float -> [%expr float_of_string [%e expr]]
      | Some (Typ t) ->
        let parse_fun = Util.mk_qualified_fun ~prefix:"parse_" ~loc t in
        [%expr [%e parse_fun] [%e expr]]
      | Some (Func (func_name, _)) ->
        let func_ident = pexp_ident ~loc { txt = func_name; loc } in
        [%expr [%e func_ident] [%e expr]]
      | Some (Pipe_all_func _) -> Util.error ~loc ">>> not allowed in destructuring let"
    in
    match List.rev bs with
    | [] -> [%expr ()]
    | bs_rev ->
      let exprs =
        List.map
          begin
            fun (_, iG, conv, _) ->
              let group_idx = match iG with None -> 0 | Some i -> i + 1 in
              apply_conv ~loc [%expr Re.Group.get _g [%e eint ~loc group_idx]] conv
          end
          bs_rev
      in
      (match exprs with [ expr ] -> expr | _ -> pexp_tuple ~loc exprs)
  in

  let default_rhs = [%expr [%e make_default_rhs ~target:`Let ~loc []]] in

  let re_var = pexp_ident ~loc { txt = Lident re_var; loc } in
  let rhs_expr =
    [%expr
      let _ppx_regexp_v = [%e expr] in
      [%e build_exec_match ~loc ~re_var ~continue_next:default_rhs ~on_match]]
    |> Regexp.check_alternation_captures ~loc r
  in

  { pvb_pat = lhs_pattern; pvb_expr = rhs_expr; pvb_attributes = []; pvb_loc = loc }, [ re_binding ]

let transform_cases ~loc cases =
  let target = `Match in
  let partition_cases cases =
    let rec partition pattern_cases = function
      | [] -> List.rev pattern_cases, []
      | ({ pc_lhs = { ppat_desc = Ppat_any | Ppat_var _; _ }; _ } as case) :: rest -> List.rev pattern_cases, case :: rest
      | case :: rest -> partition (case :: pattern_cases) rest
    in
    partition [] cases
  in

  let parse_pattern case =
    Ast_pattern.(parse (pstring __')) case.pc_lhs.ppat_loc case.pc_lhs (fun { txt = re_src; loc } ->
      let pos = Parser.calculate_pattern_pos ~loc ~pattern_str:re_src in
      let parser = Parser.get_parser ~target ~pos in
      let _, re, bs, nG, flags = Parser.run ~parser ~target ~pos re_src in
      let re_str = Pprintast.string_of_expression re in
      re, re_str, nG, bs, case.pc_rhs, case.pc_guard, flags)
  in

  let create_compilation_groups parsed_cases =
    let can_merge_into_group (_, re_str, _, _, _, guard, flags) group =
      match guard with
      | None ->
        (* no guard: all must be guard-free *)
        List.for_all (fun (_, _, _, _, _, g', _) -> g' = None) group
      | Some _ ->
        (* has guard: needs exact (RE string, flags) match *)
        List.exists (fun (_, re_str', _, _, _, _, f') -> re_str = re_str' && flags = f') group
    in

    let rec group acc current_group = function
      | [] -> if current_group = [] then List.rev acc else List.rev (List.rev current_group :: acc)
      | case :: rest ->
        if current_group = [] || can_merge_into_group case current_group then group acc (case :: current_group) rest
        else group (List.rev current_group :: acc) [ case ] rest
    in

    group [] [] parsed_cases
  in

  let process_compilation_group group_idx cases_in_group =
    (* deduplicate identical (pattern, flags) combinations *)
    let deduplicate_patterns cases =
      let add_case patterns (re, re_str, nG, bs, rhs, guard, flags) =
        let key = re_str, flags in
        let handlers = nG, bs, rhs, guard in
        let re_data = re, flags in
        match List.assoc_opt key patterns with
        | Some (re_data, existing) ->
          (key, (re_data, existing @ [ handlers ])) :: List.remove_assoc key patterns (* Append instead of prepend *)
        | None -> (key, (re_data, [ handlers ])) :: patterns
      in
      List.fold_left add_case [] cases |> List.map (fun ((_, _), (re_data, handlers)) -> re_data, handlers) |> List.rev
    in

    let add_offsets patterns =
      let rec calc acc offset = function
        | [] -> List.rev acc
        | ((re, flags), handlers) :: rest ->
          let max_captures = handlers |> List.map (fun (n, _, _, _) -> n) |> List.fold_left max 0 in
          calc (((re, flags), handlers, offset) :: acc) (offset + max_captures) rest
      in
      calc [] 0 patterns
    in

    let create_handler idx ((_, _), handlers, offset) =
      let name = Printf.sprintf "_group%d_case_%d" group_idx idx in

      let rec chain_guards = function
        | [] -> [%expr None]
        | (_, bs, rhs, None) :: _ -> [%expr Some [%e Bindings.wrap_group_bindings ~captured_acc:[] ~loc rhs offset (List.rev bs)]]
        | (_, bs, rhs, Some guard) :: rest ->
          let guarded = [%expr if [%e guard] then Some [%e rhs] else [%e chain_guards rest]] in
          Bindings.wrap_group_bindings ~captured_acc:[] ~loc guarded offset (List.rev bs)
      in

      name, [%expr fun _g -> [%e chain_guards handlers]]
    in

    let unique_patterns = deduplicate_patterns cases_in_group in
    let patterns_with_offsets = add_offsets unique_patterns in
    let handlers = List.mapi create_handler patterns_with_offsets in
    let patterns_and_flags = List.map (fun ((re, flags), _, _) -> re, flags) patterns_with_offsets in

    Util.fresh_var (), patterns_and_flags, handlers
  in

  let generate_code compilation_groups processed_groups default_rhs =
    let re_bindings =
      List.map (fun (var_name, patterns_and_flags, _) -> Re_comp.compile ~loc var_name patterns_and_flags) processed_groups
    in

    let handler_bindings =
      List.concat_map
        begin
          fun (_, _, handlers) -> List.map (fun (name, expr) -> value_binding ~loc ~pat:(ppat_var ~loc { txt = name; loc }) ~expr) handlers
        end
        processed_groups
    in

    let build_match_cascade () =
      let make_case idx ((var_name, patterns, handlers), original_group) =
        let re_var = pexp_ident ~loc { txt = Lident var_name; loc } in
        let continue = [%expr __ppx_regexp_try_next ([%e eint ~loc idx] + 1)] in
        let has_guards = List.exists (fun (_, _, _, _, _, g, _) -> g <> None) original_group in
        let is_single = match patterns with [ _ ] -> true | _ -> false in

        let on_match =
          if is_single then begin
            let handler = pexp_ident ~loc { txt = Lident (fst (List.hd handlers)); loc } in
            [%expr match [%e handler] _g with Some result -> result | None -> [%e continue]]
          end
          else begin
            let handler_array = handlers |> List.map (fun (name, _) -> pexp_ident ~loc { txt = Lident name; loc }) |> pexp_array ~loc in
            let dispatch = [%expr __ppx_regexp_dispatch (snd [%e re_var]) [%e handler_array] _g] in
            if has_guards then [%expr match [%e dispatch] with Some result -> result | None -> [%e continue]]
            else [%expr match [%e dispatch] with Some result -> result | None -> assert false]
          end
        in

        case
          ~lhs:(ppat_constant ~loc (Pconst_integer (string_of_int idx, None)))
          ~guard:None
          ~rhs:(build_exec_match ~loc ~re_var ~continue_next:continue ~on_match)
      in

      let cases = List.mapi make_case (List.combine processed_groups compilation_groups) in
      let default = case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:default_rhs in

      [%expr
        let rec __ppx_regexp_try_next group_idx = [%e pexp_match ~loc [%expr group_idx] (cases @ [ default ])] in
        __ppx_regexp_try_next 0]
    in

    let cascade = build_match_cascade () in
    if handler_bindings = [] then cascade, re_bindings else pexp_let ~loc Nonrecursive handler_bindings cascade, re_bindings
  in

  let pattern_cases, default_cases = partition_cases cases in
  let default_rhs = make_default_rhs ~target:`Match ~loc default_cases in
  if pattern_cases = [] then default_rhs, [] (* no patterns, no need for match cascading *)
  else begin
    pattern_cases |> List.map parse_pattern |> create_compilation_groups |> fun groups ->
    let processed = List.mapi process_compilation_group groups in
    generate_code groups processed default_rhs
  end

let transform_mixed_match ~loc ?matched_expr cases acc =
  let target = `Match in
  let aux case =
    match case.pc_lhs.ppat_desc with
    | Ppat_extension
        ( { txt = "mikmatch"; _ },
          PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (pat, str_loc, _)); _ }, _); _ } ] ) ->
      let pos = str_loc.loc_start in
      let parser = Parser.get_parser ~target ~pos in
      let _, re, bs, nG, flags = Parser.run ~parser ~pos ~target pat in
      `Ext (re, nG, bs, case.pc_rhs, case.pc_guard, flags)
    | _ -> `Regular case
  in

  let prepared_cases = List.map aux cases in
  let has_ext = List.exists (function `Ext _ -> true | _ -> false) prepared_cases in

  if not has_ext then (match matched_expr with None -> pexp_function ~loc cases, acc | Some m -> pexp_match ~loc m cases, acc)
  else begin
    let compilations =
      prepared_cases
      |> List.mapi (fun i case ->
           match case with
           | `Ext (re, _, _, _, _, flags) ->
             let comp_var = Util.fresh_var () in
             let comp_expr = Re_comp.compile_single ~loc re flags in
             let binding = value_binding ~loc ~pat:(ppat_var ~loc { txt = comp_var; loc }) ~expr:comp_expr in
             Some (i, comp_var, binding)
           | _ -> None)
      |> List.filter_map (fun x -> x)
    in

    let bindings = List.map (fun (_, _, b) -> b) compilations in

    let default_rhs = make_default_rhs ~target ~loc [] in

    let rec build_ordered_match input_var case_idx cases comps =
      match cases, comps with
      | [], _ -> default_rhs
      | `Regular case :: rest, _ ->
        [%expr
          match [%e input_var] with
          | [%p case.pc_lhs] when [%e Option.value case.pc_guard ~default:[%expr true]] -> [%e case.pc_rhs]
          | _ -> [%e build_ordered_match input_var (case_idx + 1) rest comps]]
      | `Ext (_, _, bs, rhs, guard, _) :: rest, (idx, comp_var, _) :: rest_comps when idx = case_idx ->
        let comp_ident = pexp_ident ~loc { txt = Lident comp_var; loc } in
        [%expr
          match Re.exec_opt [%e comp_ident] [%e input_var] with
          | Some _g ->
            [%e
              let bs = List.rev bs in
              match guard with
              | None -> Bindings.wrap_group_bindings ~captured_acc:[] ~loc rhs 0 bs
              | Some g ->
                let guarded_rhs = [%expr if [%e g] then [%e rhs] else [%e build_ordered_match input_var (case_idx + 1) rest rest_comps]] in
                Bindings.wrap_group_bindings ~captured_acc:[] ~loc guarded_rhs 0 bs]
          | None -> [%e build_ordered_match input_var (case_idx + 1) rest rest_comps]]
      | `Ext _ :: rest, _ -> build_ordered_match input_var (case_idx + 1) rest comps
    in

    let match_body = build_ordered_match [%expr _ppx_regexp_v] 0 prepared_cases compilations in
    let match_expr =
      match matched_expr with
      | None -> [%expr fun _ppx_regexp_v -> [%e match_body]]
      | Some m ->
        [%expr
          let _ppx_regexp_v = [%e m] in
          [%e match_body]]
    in
    match_expr, bindings @ acc
  end

let transform_type ~loc rec_flag type_name pattern_str _td =
  let pos = Parser.calculate_pattern_pos ~loc ~pattern_str in
  let parser = Parser.get_parser ~target:`Let ~pos in
  let r, re, bs, nG, flags = Parser.run ~parser ~target:`Let ~pos pattern_str in
  let re_no_groups = Regexp.to_re_expr ~group:false ~in_let:false r in
  if nG = 0 then Util.error ~loc "You must have at least one capture to create a valid record."
  else begin
    let fields =
      List.map
        (fun (name, idx, conv, must) ->
          if name.txt = "_" then None
          else (
            let field_type =
              let base_type =
                match conv with
                | None -> [%type: string]
                | Some Regexp_types.Int -> [%type: int]
                | Some Float -> [%type: float]
                | Some (Typ t) -> ptyp_constr ~loc { txt = t; loc } []
                | Some (Func (_, Some typ)) -> ptyp_constr ~loc { txt = typ; loc } []
                | Some (Func (_, None)) -> [%type: string]
                | Some (Pipe_all_func _) -> Util.error ~loc ">>> not allowed in type definitions"
              in
              if must then base_type else [%type: [%t base_type] option]
            in
            Some (name, idx, field_type, conv, must)))
        (List.rev bs)
      |> List.filter_map (fun x -> x)
    in

    let record_fields =
      List.map
        (fun (name, _, typ, _, _) -> label_declaration ~loc ~name:{ txt = name.txt; loc = name.loc } ~mutable_:Immutable ~type_:typ)
        fields
    in

    let type_decl =
      type_declaration ~loc ~name:{ txt = type_name; loc } ~params:[] ~cstrs:[] ~kind:(Ptype_record record_fields) ~private_:Public
        ~manifest:None
    in

    let parse_func_name = "parse_" ^ type_name in
    let parse_exn_func_name = "parse_" ^ type_name ^ "_exn" in
    let re_var = Util.fresh_var () in

    let build_record =
      let field_exprs =
        List.mapi
          (fun i (name, _, _, conv, must) ->
            let group_idx = i + 1 in
            let group_expr = [%expr Re.Group.get _g [%e eint ~loc group_idx]] in
            let converted =
              match conv with
              | None -> group_expr
              | Some Regexp_types.Int -> [%expr int_of_string [%e group_expr]]
              | Some Float -> [%expr float_of_string [%e group_expr]]
              | Some (Typ t) ->
                let parse_fun = Util.mk_qualified_fun ~loc ~prefix:"parse_" ~suffix:"_exn" t in
                [%expr [%e parse_fun] [%e group_expr]]
              | Some (Func (func_name, _)) ->
                let func = pexp_ident ~loc { txt = func_name; loc } in
                [%expr [%e func] [%e group_expr]]
              | _ -> group_expr
            in
            let value = if must then converted else [%expr try Some [%e converted] with Not_found -> None] in
            { txt = Lident name.txt; loc }, value)
          fields
      in
      pexp_record ~loc field_exprs None
    in

    let parse_binding = Re_comp.compile ~loc re_var [ re, flags ] in

    let unescape_literal s =
      let rec unescape acc i =
        if i >= String.length s then String.concat "" (List.rev acc)
        else if i + 1 < String.length s && s.[i] = '\\' then (
          match s.[i + 1] with
          | ('[' | ']' | '(' | ')' | '{' | '}' | '.' | '*' | '+' | '?' | '|' | '^' | '$') as c -> unescape (String.make 1 c :: acc) (i + 2)
          | _ -> unescape (String.sub s i 1 :: acc) (i + 1))
        else unescape (String.sub s i 1 :: acc) (i + 1)
      in
      unescape [] 0
    in

    let pp_func_name = "pp_" ^ type_name in

    let rec build_pp_expr (node : _ Location.loc) =
      match node.txt with
      | Regexp_types.Code s -> [%expr Format.pp_print_string ppf [%e estring ~loc @@ unescape_literal s]]
      | Seq es ->
        let exprs = List.map build_pp_expr es in
        List.fold_left
          (fun acc e ->
            [%expr
              [%e acc];
              [%e e]])
          [%expr ()] exprs
      | Alt branches ->
        (* branch selection based on populated fields *)
        build_alt_pp_expr branches
      | Capture_as (name, conv, _) when name.txt <> "_" ->
        let field_access = pexp_field ~loc [%expr v] { txt = Lident name.txt; loc } in
        let must = List.exists (fun (n, _, _, _, m) -> n.txt = name.txt && m) fields in
        let print_expr =
          let base =
            match conv with
            | Some (Func (_func_name, Some typ)) ->
              let pp_name = match typ with Lident n -> "pp_" ^ n | _ -> "pp_custom" in
              [%expr [%e pexp_ident ~loc { txt = Lident pp_name; loc }] ppf]
            | Some Regexp_types.Int -> [%expr Format.fprintf ppf "%d"]
            | Some Float -> [%expr Format.fprintf ppf "%g"]
            | Some (Typ t) ->
              let pp_fun = Util.mk_qualified_fun ~loc ~prefix:"pp_" t in
              [%expr Format.fprintf ppf "%a" [%e pp_fun]]
            | _ -> [%expr Format.pp_print_string ppf]
          in
          if must then [%expr [%e base] [%e field_access]] else [%expr match [%e field_access] with None -> () | Some x -> [%e base] x]
        in
        print_expr
      | Opt e ->
        let rec find_capture = function
          | { txt = Regexp_types.Capture_as (name, _, _); _ } -> Some name.txt
          | { txt = Seq es; _ } -> List.find_map find_capture es
          | _ -> None
        in
        begin
          match find_capture e with
          | Some name ->
            let field_access = pexp_field ~loc [%expr v] { txt = Lident name; loc } in
            [%expr match [%e field_access] with None -> () | Some _ -> [%e build_pp_expr e]]
          | None -> build_pp_expr e
        end
      | Repeat (range, e) ->
        let min_reps, max_reps_opt = range.txt in
        begin
          match max_reps_opt with
          | Some max when min_reps <> max ->
            (* try to determine actual count from context *)
            [%expr
              let count = [%e eint ~loc min_reps] in
              for _ = 1 to count do
                [%e build_pp_expr e]
              done]
          | _ ->
            let rec repeat_n n expr =
              if n <= 0 then [%expr ()]
              else if n = 1 then expr
              else
                [%expr
                  [%e expr];
                  [%e repeat_n (n - 1) expr]]
            in
            repeat_n min_reps (build_pp_expr e)
        end
      | _ -> [%expr ()]
    (* determine branch based on populated fields *)
    and build_alt_pp_expr branches =
      let rec get_captures = function
        | { txt = Regexp_types.Capture_as (name, _, e); _ } -> name.txt :: get_captures e
        | { txt = Seq es; _ } -> List.concat_map get_captures es
        | { txt = Opt e; _ } -> get_captures e
        | _ -> []
      in

      let branch_conditions =
        List.map
          (fun branch ->
            let captures = get_captures branch in
            let condition =
              match captures with
              | [] -> [%expr false]
              | names ->
                List.map
                  (fun name ->
                    let field = pexp_field ~loc [%expr v] { txt = Lident name; loc } in
                    let is_field_defined = List.exists (fun (n, _, _, _, _) -> n.txt = name) fields in
                    if is_field_defined then [%expr match [%e field] with None -> false | Some _ -> true] else [%expr false])
                  names
                |> ( function
                | [] -> [%expr false]
                | [ cond ] -> cond
                | conds -> List.fold_left (fun acc cond -> [%expr [%e acc] || [%e cond]]) (List.hd conds) (List.tl conds) )
            in
            condition, build_pp_expr branch)
          branches
      in

      let rec build_cascade = function
        | [] -> [%expr ()]
        | [ (_, expr) ] -> expr
        | (cond, expr) :: rest -> [%expr if [%e cond] then [%e expr] else [%e build_cascade rest]]
      in

      build_cascade branch_conditions
    in

    let pp_body = build_pp_expr r in

    let re_binding = value_binding ~loc ~pat:(ppat_var ~loc { txt = type_name; loc }) ~expr:re_no_groups in

    let default_rhs = make_default_rhs ~target:`Let ~loc [] in

    let items =
      [
        pstr_type ~loc rec_flag [ type_decl ];
        pstr_value ~loc Nonrecursive [ parse_binding ];
        [%stri
          let[@warning "-32"] [%p ppat_var ~loc { txt = parse_func_name; loc }] =
           fun s ->
            match Re.exec_opt (fst [%e pexp_ident ~loc { txt = Lident re_var; loc }]) s with
            | None -> None
            | Some _g -> Some [%e build_record]];
        [%stri
          let[@warning "-32"] [%p ppat_var ~loc { txt = parse_exn_func_name; loc }] =
           fun s -> match [%e pexp_ident ~loc { txt = Lident parse_func_name; loc }] s with None -> [%e default_rhs] | Some t -> t];
        [%stri let[@warning "-32"] [%p ppat_var ~loc { txt = pp_func_name; loc }] = fun ppf v -> [%e pp_body]];
      ]
    in
    items, re_binding
  end
