open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

exception Empty_record
exception Not_a_record
exception No_type_name

let to_prod_mapper argv =
  { default_mapper with
    structure = fun mapper strs ->
      List.fold_right (fun str acc ->
          match str.pstr_desc with
          | Pstr_type(types) ->
            let new_types, new_func = List.fold_right (fun type_decl (acc, acc_fun) ->
                let attribs = type_decl.ptype_attributes in
                let new_attribs, acc_fun =
                  List.fold_right (fun a (acc, new_fun) -> match a with
                    | {txt = "to_prod"; loc}, a ->
                      begin
                        match type_decl.ptype_kind with
                        | Ptype_record(l) ->
                          let all_vars =  List.sort (fun l1 l2 -> String.compare l2.pld_name.txt l1.pld_name.txt) l in (* the reverse is intended *)
                          let all_names = List.map (fun l -> l.pld_name) all_vars in
                          let arg = Exp.ident { txt = Longident.Lident "myarg"; loc} in
                          let big_tuple =
                            if List.length all_names = 0 then raise Empty_record
                            else
                            List.fold_left (fun exp l ->
                                  Exp.tuple [Exp.field arg { txt = Longident.Lident l.txt; loc}; exp]
                                ) (Exp.field arg { txt = Longident.Lident (List.hd all_names).txt; loc}) (List.tl all_names) in
                          (*let to_type = function
                          let object_type_tuple =
                            if List.length all_vars = 0 then raise Empty_record
                            else
                            List.fold_left (fun exp l ->
                                  Exp.tuple [Exp.field arg { txt = Longident.Lident l.txt; loc}; exp]
                                ) (Exp.field arg { txt = Longident.Lident (List.hd all_vars).txt; loc}) (List.tl all_vars) in*)
                          let fun_name = Pat.var { txt = type_decl.ptype_name.txt ^ "_to_prod"; loc } in
                          let access_example =
                            Str.value Nonrecursive [Vb.mk fun_name (Exp.fun_ "" None (Pat.var {txt = "myarg"; loc; }) big_tuple)]
                          in
                          (acc, access_example :: new_fun)
                        | _ -> raise Not_a_record
                      end
                    | x -> (x :: acc, new_fun)
                    ) attribs ([], acc_fun)
                in
                ({ type_decl with ptype_attributes = new_attribs }) :: acc, acc_fun
              ) types ([], []) in
            { str with pstr_desc = Pstr_type (new_types)} :: (new_func @  acc)
          | x -> str :: acc

        ) strs []
      |> default_mapper.structure mapper
  }

let () = register "to_prod" to_prod_mapper
