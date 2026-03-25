(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the  LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ppxlib
open Ast_builder.Default

let pvar ~loc name = ppat_var ~loc { txt = name; loc }
let evar ~loc name = pexp_ident ~loc { txt = Lident name; loc }
let rec list_take n = function [] -> [] | x :: xs -> if n <= 0 then [] else x :: list_take (n - 1) xs
let list_is_empty = function [] -> true | _ -> false

let make_alias_binding ~loc ~var_name =
  let pat = pvar ~loc var_name in
  let expr = evar ~loc var_name in
  match [%stri let[@warning "-32"] [%p pat] = [%e expr]] with { pstr_desc = Pstr_value (_, [ vb ]); _ } -> vb | _ -> assert false

type binding_location =
  | TopLevel
  | InModule of string list (* module path *)

let transformation =
  object (self)
    inherit [(binding_location * value_binding) list] Ast_traverse.fold_map as super

    method! structure_item item acc =
      match item.pstr_desc with
      | Pstr_type (rec_flag, type_decls) ->
        let needs_transformation =
          List.exists
            (fun td ->
              match td.ptype_manifest with Some { ptyp_desc = Ptyp_extension ({ txt = "mikmatch"; _ }, _); _ } -> true | _ -> false)
            type_decls
        in

        if not needs_transformation then super#structure_item item acc
        else (
          let all_items, all_bindings =
            List.fold_left
              (fun (items_acc, bindings_acc) td ->
                match td.ptype_manifest with
                (* type ... = {%mikmatch| ... |} *)
                | Some
                    {
                      ptyp_desc =
                        Ptyp_extension
                          ( { txt = "mikmatch"; _ },
                            PStr
                              [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (pattern_str, loc, _)); _ }, _); _ } ]
                          );
                      _;
                    } ->
                  let type_name = td.ptype_name.txt in
                  let items, binding = Transformations.transform_type ~loc rec_flag type_name pattern_str td in
                  let alias = pstr_value ~loc Nonrecursive [ make_alias_binding ~loc ~var_name:type_name ] in
                  (alias :: items_acc) @ items, (TopLevel, binding) :: bindings_acc
                | _ -> items_acc, bindings_acc)
              ([], acc) type_decls
          in

          let wrapped = pstr_include ~loc:item.pstr_loc (include_infos ~loc:item.pstr_loc (pmod_structure ~loc:item.pstr_loc all_items)) in
          wrapped, all_bindings)
      (* let%mikmatch ... = {| ... |}*)
      | Pstr_extension (({ txt = "mikmatch"; _ }, PStr [ { pstr_desc = Pstr_value (rec_flag, vbs); _ } ]), _) ->
        let processed_vbs, collected_bindings =
          List.fold_left
            begin fun (vbs_acc, bindings_acc) vb ->
              match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
              (* pattern definition - let%mikmatch/%pcre name = {|/regex/|} *)
              | Ppat_var { txt = var_name; _ }, Pexp_constant (Pconst_string (_, loc, _)) ->
                let binding = Transformations.transform_let ~loc vb in
                let alias = make_alias_binding ~loc ~var_name in
                alias :: vbs_acc, (TopLevel, binding) :: bindings_acc
              (* destructuring - let%mikmatch {|/pattern/|} = expr *)
              | Ppat_constant (Pconst_string (pattern_str, _, _)), _ ->
                let new_vb, new_bindings = Transformations.transform_destructuring_let ~loc:vb.pvb_loc pattern_str vb.pvb_expr in
                new_vb :: vbs_acc, List.map (fun b -> TopLevel, b) new_bindings @ bindings_acc
              | _ -> vbs_acc, bindings_acc
            end
            ([], acc) vbs
        in

        let new_item = { item with pstr_desc = Pstr_value (rec_flag, List.rev processed_vbs) } in
        new_item, collected_bindings
      (* let ... = expression (which might contain %mikmatch)
         e.g. let ... = {%mikmatch| ... |}
      *)
      | Pstr_value (rec_flag, vbs) ->
        let processed_vbs, collected_bindings =
          List.fold_left
            (fun (vbs_acc, bindings_acc) vb ->
              match vb.pvb_expr.pexp_desc with
              | Pexp_extension ({ txt = "mikmatch"; loc }, PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ])
                when match expr.pexp_desc with Pexp_constant (Pconst_string _) -> true | _ -> false ->
                let new_vb = { vb with pvb_expr = expr } in
                let binding = Transformations.transform_let ~loc new_vb in
                let alias =
                  match vb.pvb_pat.ppat_desc with Ppat_var { txt = var_name; loc } -> make_alias_binding ~loc ~var_name | _ -> new_vb
                in
                alias :: vbs_acc, (TopLevel, binding) :: bindings_acc
              | _ ->
                let new_expr, new_bindings = self#expression vb.pvb_expr bindings_acc in
                let new_vb = { vb with pvb_expr = new_expr } in
                new_vb :: vbs_acc, new_bindings)
            ([], acc) vbs
        in
        let new_item = { item with pstr_desc = Pstr_value (rec_flag, List.rev processed_vbs) } in
        new_item, collected_bindings
      (* module M = struct ... end which might contain %mikmatch defns *)
      | Pstr_module { pmb_name = { txt = Some mod_name; _ } as name; pmb_expr; pmb_attributes; pmb_loc } -> begin
        match pmb_expr.pmod_desc with
        | Pmod_structure mod_items ->
          let mod_items', mod_bindings = self#structure mod_items [] in
          if mod_bindings = [] then super#structure_item item acc
          else (
            let tagged_bindings =
              List.map
                (fun (loc, vb) -> match loc with InModule path -> InModule (mod_name :: path), vb | TopLevel -> InModule [ mod_name ], vb)
                mod_bindings
            in

            (* include the module from the prelude struct, but only if we tagged bingings, then keep other original items *)
            let include_item =
              pstr_include ~loc:pmb_loc (include_infos ~loc:pmb_loc (pmod_ident ~loc:pmb_loc { txt = Lident mod_name; loc = pmb_loc }))
            in
            let new_items = if list_is_empty tagged_bindings then mod_items' else include_item :: mod_items' in

            let alias_module =
              pstr_module ~loc:pmb_loc { pmb_name = name; pmb_expr = pmod_structure ~loc:pmb_loc new_items; pmb_attributes; pmb_loc }
            in
            alias_module, tagged_bindings @ acc)
        | _ ->
          (* other module types, no transformation needed *)
          super#structure_item item acc
      end
      | _ -> super#structure_item item acc

    method! expression e_ext acc =
      let e_ext, acc = super#expression e_ext acc in
      let has_ext_case =
        List.exists begin fun case ->
          match case.pc_lhs.ppat_desc with Ppat_extension ({ txt = "mikmatch"; _ }, _) -> true | _ -> false
        end
      in
      match e_ext.pexp_desc with
      (* match%mikmatch and function%mikmatch *)
      | Pexp_extension ({ txt = "mikmatch"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
        let loc = e.pexp_loc in
        begin match e.pexp_desc with
        | Pexp_function ([], _, Pfunction_cases (cases, _, _)) ->
          let cases, binding = Transformations.transform_cases ~loc cases in
          [%expr fun _ppx_mikmatch_v -> [%e cases]], List.map (fun b -> TopLevel, b) binding @ acc
        | Pexp_match (e, cases) ->
          let cases, binding = Transformations.transform_cases ~loc cases in
          ( [%expr
              let _ppx_mikmatch_v = [%e e] in
              [%e cases]],
            List.map (fun b -> TopLevel, b) binding @ acc )
        | Pexp_let (rec_flag, vbs, body) ->
          let processed_vbs, new_bindings =
            List.fold_left
              (fun (vbs_acc, bindings_acc) vb ->
                match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
                | Ppat_constant (Pconst_string (pattern_str, _, _)), _ ->
                  let new_vb, new_bindings = Transformations.transform_destructuring_let ~loc:vb.pvb_loc pattern_str vb.pvb_expr in
                  new_vb :: vbs_acc, List.map (fun b -> TopLevel, b) new_bindings @ bindings_acc
                | _ ->
                  Util.error ~loc
                    "[%%pcre] and [%%mikmatch] only apply to match, function, global let declarations of strings, and let destructuring.")
              ([], []) vbs
          in
          pexp_let ~loc rec_flag (List.rev processed_vbs) body, new_bindings @ acc
        | _ ->
          Util.error ~loc
            "[%%pcre] and [%%mikmatch] only apply to match, function, global let declarations of strings, and let destructuring."
        end
      (* match smth with | {%mikmatch|some regex|} -> ...*)
      | Pexp_match (matched_expr, cases) when has_ext_case cases ->
        let plain_acc = List.map snd acc in
        let expr, bindings = Transformations.transform_mixed_match ~loc:e_ext.pexp_loc ~matched_expr cases plain_acc in
        expr, List.map (fun b -> TopLevel, b) bindings
      | Pexp_function (params, constraint_, Pfunction_cases (cases, _, _)) when has_ext_case cases ->
        let plain_acc = List.map snd acc in
        let transformed, bindings = Transformations.transform_mixed_match ~loc:e_ext.pexp_loc cases plain_acc in
        let acc = List.map (fun b -> TopLevel, b) bindings in
        begin match params with
        | [] -> transformed, acc
        | _ -> { e_ext with pexp_desc = Pexp_function (params, constraint_, Pfunction_body transformed) }, acc
        end
      | _ -> e_ext, acc
  end

let dispatch_function_binding ~loc =
  let open Ppxlib in
  let open Ast_builder.Default in
  value_binding ~loc
    ~pat:(ppat_var ~loc { txt = "__ppx_mikmatch_dispatch"; loc })
    ~expr:
      [%expr
        fun marks handlers _g ->
          let rec loop i =
            if i >= Array.length marks then None
            else if Re.Mark.test _g marks.(i) then (match handlers.(i) _g with Some result -> Some result | None -> loop (i + 1))
            else loop (i + 1)
          in
          loop 0]

let impl str =
  let str, rev_bindings = transformation#structure str [] in
  match rev_bindings with
  | [] -> str
  | _ -> begin
    let loc = match List.hd (List.rev rev_bindings) with _, { pvb_loc; _ } -> pvb_loc in

    let bindings = List.rev rev_bindings in

    (* process all bindings in source order, build modules as needed *)
    let rec emit_in_order remaining =
      match remaining with
      | [] -> []
      | (TopLevel, vb) :: rest ->
        (* emit top-level binding immediately *)
        let items = [%str let[@warning "-32"] [%p vb.pvb_pat] = [%e vb.pvb_expr]] in
        items @ emit_in_order rest
      | (InModule path, vb) :: rest ->
        let root = List.hd path in
        (* collect all consecutive bindings for this root module *)
        let same_root, different =
          let rec collect acc = function
            | ((InModule p, _) as b) :: rest when List.hd p = root -> collect (b :: acc) rest
            | rest -> List.rev acc, rest
          in
          collect [ InModule path, vb ] rest
        in
        let mod_items = build_module_tree ~loc root same_root in
        mod_items @ emit_in_order different
    and build_module_tree ~loc root module_bindings =
      let by_path = Hashtbl.create 16 in
      List.iter
        begin fun binding ->
          match binding with
          | InModule path, vb ->
            let existing = try Hashtbl.find by_path path with Not_found -> [] in
            Hashtbl.replace by_path path (vb :: existing)
          | TopLevel, _ -> assert false
        end
        module_bindings;

      let rec build_at_path current_path =
        let direct_bindings = try Hashtbl.find by_path current_path |> List.rev with Not_found -> [] in
        let direct_items = List.concat_map (fun vb -> [%str let[@warning "-32"] [%p vb.pvb_pat] = [%e vb.pvb_expr]]) direct_bindings in

        let child_modules =
          Hashtbl.fold
            begin fun path _ acc ->
              if
                List.length path = List.length current_path + 1
                && List.for_all2 ( = ) current_path (list_take (List.length current_path) path)
              then List.nth path (List.length current_path) :: acc
              else acc
            end
            by_path []
          |> List.sort_uniq compare
        in

        (* build nested modules *)
        let nested_items =
          List.concat_map
            (fun child_name ->
              let child_path = current_path @ [ child_name ] in
              let child_items = build_at_path child_path in
              [
                pstr_module ~loc
                  {
                    pmb_name = { txt = Some child_name; loc };
                    pmb_expr = pmod_structure ~loc child_items;
                    pmb_attributes = [];
                    pmb_loc = loc;
                  };
              ])
            child_modules
        in

        direct_items @ nested_items
      in

      let mod_body = build_at_path [ root ] in
      [
        pstr_module ~loc
          { pmb_name = { txt = Some root; loc }; pmb_expr = pmod_structure ~loc mod_body; pmb_attributes = []; pmb_loc = loc };
      ]
    in

    let struct_items = [%str [%%i pstr_value ~loc Nonrecursive [ dispatch_function_binding ~loc ]]] @ emit_in_order bindings in

    let mod_expr = pmod_structure ~loc struct_items in
    [%str open [%m mod_expr]] @ str
  end

let () = Driver.register_transformation ~impl "ppx_mikmatch"
