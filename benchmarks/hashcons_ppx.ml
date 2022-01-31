open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let print_queue q = 
  let print q =
      Queue.iter (fun e -> print_string (e^", ")) q
  in print_string "(";print q;print_string ") "

type func = Func of string * string list * Parsetree.expression


type ident = {
  identName: string;
  identType: string;
  func: Longident.t Asttypes.loc
}

type constructor = {
  consName: string;
  idents: ident list
}

type customType = {
  typeName: string;
  constructors: constructor list
}
let defaultFunc = {txt = Lident ""; loc=(!default_loc)}

let customType = ref {typeName = ""; constructors = []} 

let printIdent ident =
  print_string "(";
  print_string ident.identName;
  print_string ", ";
  print_string ident.identType;
  print_string ", [";
  (* print_string "Funcs length: ";
  List.length (snd ident.idents); *)
  print_string "])";
  print_newline ()

let printConstructor const =
  print_string ("name = "^const.consName^"\n");
  print_string ("list length = ");
  print_int (List.length const.idents);
  print_newline ();
  List.iter (fun x -> printIdent x) (const.idents)

let printCType type_ =
  print_string "{\n";
  print_string ("typeName = "^type_.typeName^"\n");
  List.iter printConstructor type_.constructors;
  print_string "\n}"
  
(* let type_name = ref "" *)
(* let id_names = Queue.create () *)

(* let constructTuple l =
  let rec constructList l =
    match l with
    | [] -> []
    | x::xs -> (Exp.construct ({txt = Lident x; loc=(!default_loc)}) None) :: (constructList xs)
  in Exp.tuple (constructList l) *)

(* let isSingleArg args = if List.length args = 1 then true else false *)

(* let writeCacheArgs args =
  if isSingleArg args
  then Exp.ident {txt = Lident (List.hd args); loc=(!default_loc)}
  else Exp.ident {txt = Lident "arg"; loc=(!default_loc)} *)

(* let seq args = Exp.sequence
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)}))
    [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
     (Nolabel, writeCacheArgs args);
     (Nolabel, Exp.ident {txt = Lident "res"; loc=(!default_loc)})]
  )
  (Exp.ident {txt = Lident "res"; loc=(!default_loc)}) *)

(* let letyBinding expression = Vb.mk (Pat.var {txt = "res"; loc=(!default_loc)}) (expression) *)

(* let matchRight expression args = Exp.let_ Nonrecursive [(letyBinding expression)] (seq args)   *)

(* let withCase expression args = 
  {
    pc_lhs = Pat.construct {txt = Lident "Not_found"; loc=(!default_loc)}
  None; 
  	pc_guard = None;
  	pc_rhs = (matchRight expression args)
  } *)

(* let cacheFind args = Exp.apply
  (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "find"); loc=(!default_loc)}))
  [(Nolabel, Exp.ident {txt = Lident "cache"; loc=(!default_loc)});
   (Nolabel, writeCacheArgs args)] *)

(* let tryExp expression args = Exp.try_ (cacheFind args) [(withCase expression args)] *)

(* let rec gExp expression args =
  let keepArgs = args in
  let rec writeArgs = function
  | [] -> begin
            if isSingleArg args
            then tryExp expression keepArgs
            else Exp.let_ Nonrecursive [(Vb.mk (Pat.var {txt = "arg"; loc=(!default_loc)}) (constructTuple args))] (tryExp expression keepArgs)
          end
  | x::xs -> Exp.fun_ Nolabel None ((Pat.var {txt = x; loc=(!default_loc)})) (writeArgs xs)
  in writeArgs args *)

(* let g rec_flag funName expression args =
  Exp.let_
  rec_flag
  [Vb.mk (Pat.var {txt = funName; loc=(!default_loc)}) (gExp expression args)]
  (Exp.fun_ Nolabel None (Pat.var {txt = "x"; loc=(!default_loc)})
  (Exp.apply (Exp.ident {txt = Lident funName; loc=(!default_loc)}) [(Nolabel, Exp.ident {txt = Lident "x"; loc=(!default_loc)})])) *)
  (*(Exp.ident {txt = Lident funName; loc=(!default_loc)})*)

(* let cacheCreate = Vb.mk (Pat.var {txt = "cache"; loc=(!default_loc)})
  (Exp.apply
    (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)}))
    [(Nolabel, Exp.constant (Pconst_integer ("16", None)))]
  ) *)

(* let memoExp rec_flag funName expression args = Exp.let_ Nonrecursive [cacheCreate] (g rec_flag funName expression args) *)

(* let fix_memo rec_flag funcList = 
  let rec writeFuncs funcList =
    match funcList with
    | [] -> []
    | Func(name, args, expr)::xs -> Vb.mk (Pat.var {txt = name; loc=(!default_loc)}) (memoExp rec_flag name expr args)::(writeFuncs xs)
  in Str.value rec_flag (writeFuncs funcList) *)

let rec getFuncArgsAndBody functionName args expr =
  match expr with
  | {pexp_desc = pexp;_} -> 
    begin
      match pexp with
      | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = arg;_};_}, body) -> getFuncArgsAndBody functionName (arg::args) body
      | _ -> if List.length args = 0
             then raise (Location.Error (Location.error ("The function "^functionName^" must have at least one argument!")))
             else Func(functionName, (List.rev args), expr)
    end

let rec createFunc functions =    
  match functions with
  | [] -> []
  | {pvb_pat = pat; pvb_expr = expr;_}::xs -> (Vb.mk pat (
    Exp.apply (Exp.ident {txt = Lident "hashcons"; loc=(!default_loc)})
    [(Nolabel, expr)]))::(createFunc xs)
  | {pvb_pat = {ppat_desc = (Ppat_any | Ppat_alias (_, _) | Ppat_constant _| Ppat_interval (_, _) | Ppat_tuple _ | Ppat_construct (_, _) | Ppat_variant (_, _) | Ppat_record (_, _) | Ppat_array _| Ppat_or (_, _) | Ppat_constraint (_, _) | Ppat_type _ | Ppat_lazy _ | Ppat_unpack _ | Ppat_exception _ | Ppat_extension _ | Ppat_open (_, _)); _ }; _ }::_ -> raise (Location.Error (Location.error "Syntax error in expression mapper"))

let rec separateTypeStr l =
  let rec getTypeStr l =
    match l with
    | [] -> []
    | {pstr_desc = Pstr_type (a, b); _}::xs -> (Str.type_ a b):: getTypeStr xs
    | _::xs -> getTypeStr xs
  in
  let rec getNonTypeStr l =
    match l with
    | [] -> []
    | {pstr_desc = Pstr_type _; _}::xs -> getNonTypeStr xs
    | x::xs -> x :: getNonTypeStr xs
  in (getTypeStr l, getNonTypeStr l)

  (* match args with
  | [] -> []
  | [{ptyp_desc = Ptyp_constr ({txt = Lident ident; _}, []); _}] -> [(ident, defaultFunc)]
  | {ptyp_desc = Ptyp_extension({ txt = "hcons"; loc }, pstr); _}::{ptyp_desc = Ptyp_constr ({txt = Lident idType; _}, []); ptyp_attributes = attrs; _}::xs ->
    begin
      match pstr with
      | PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_ident {txt = Lident id_name; _}; _}, _); _}] -> (id_name, getFunction attrs)::getIdAndFunct xs
      | _ -> getIdAndFunct xs
    end
  | x::xs -> getIdAndFunct xs *)

let searchCustomId args =
  match args with
  (* | [] -> never happens *)
  | { ptyp_desc = Ptyp_extension({ txt = "hcons"; loc }, pstr); _}::{ptyp_desc = Ptyp_constr ({txt = Lident idType; _}, []); ptyp_attributes = attrs; _}::xs ->
    begin
      match pstr with
      | PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_ident {txt = Lident id_name; _}; _}, _); _}] -> (Typ.constr ({txt = Lident idType; loc=(!default_loc)}) [])::xs
      | _ -> args
    end
  | _ -> args

  let rec modifyTypeArg variants =
    match variants with
    | [] -> []
    | {pcd_name = name; pcd_args = Pcstr_tuple []; _}::xs -> Type.constructor name::modifyTypeArg xs
    | {pcd_name = name; pcd_args = Pcstr_tuple args; _}::xs ->
      let newArgs = searchCustomId args in
      (* print_queue id_names; *)
        (Type.constructor ~args:(Pcstr_tuple (Typ.mk (Ptyp_constr ({txt = Lident "int"; loc=(!default_loc)}, []))::newArgs)) name)::modifyTypeArg xs
    | x -> x
(* let addInteger t =
  match t with
  | {pstr_desc =
   Pstr_type (rec_flag,
    [{ptype_name = name;
      ptype_params = _;
      ptype_cstrs = _;
      ptype_kind = Ptype_variant variants;
      ptype_private = _;
      ptype_manifest = _
      ;_}]); _} -> let x = Str.type_ rec_flag [Type.mk ~kind:(Ptype_variant (modifyTypeArg variants)) name] in
      print_string "\n";
      print_int (Queue.length id_names);print_queue id_names;
       x
  | _ -> t *)

(* type constructor = {
  name: string;
  idents: (string * Longident.t Asttypes.loc) list
}

type customType = {
  typeName: string;
  constructors: constructor list
} *)

let writeCustomID id = if String.length id = 0 then "u" else id

let rec writeIdents idents =
  match idents with
  | [] -> []
  | _::xs -> Pat.any () :: writeIdents xs

let writeCustomIDPat idents =
  if List.length idents = 0 
  then None
  else Some (Pat.tuple (Pat.var {txt = writeCustomID (List.hd idents).identName; loc=(!default_loc)} :: (writeIdents idents)))

let writeCustomIDExpr idents =
  if List.length idents = 0 
  then (Exp.constant (Pconst_integer ("0", None)))
  else (Exp.ident {txt = Lident (writeCustomID (List.hd idents).identName); loc=(!default_loc)})

let unique constructors =
  let rec newUniqueAux constructors =
    match constructors with
    | [] ->  []
    | x::xs -> Exp.case (Pat.construct {txt = Lident x.consName; loc=(!default_loc)} (writeCustomIDPat x.idents)) (writeCustomIDExpr x.idents) :: newUniqueAux xs
  in Str.value Nonrecursive [Vb.mk (Pat.var {txt = "unique"; loc=(!default_loc)}) (Exp.function_ (newUniqueAux constructors))]
(* [
  Exp.case (Pat.construct {txt = Lident "E"; loc=(!default_loc)} None)
  
  (Exp.constant (Pconst_integer ("0", None)));

 Exp.case (Pat.construct {txt = Lident "N"; loc=(!default_loc)} (Some (Pat.tuple [(Pat.var {txt = "u"; loc=(!default_loc)}); Pat.any (); Pat.any (); Pat.any ()]))) 
 
 (Exp.ident {txt = Lident "u"; loc=(!default_loc)})] *)

(* let unique = Str.value Nonrecursive [Vb.mk (Pat.var {txt = "unique"; loc=(!default_loc)}) (Exp.function_ [Exp.case (Pat.construct {txt = Lident "E"; loc=(!default_loc)} None) (Exp.constant (Pconst_integer ("0", None)));
 Exp.case (Pat.construct {txt = Lident "N"; loc=(!default_loc)} (Some (Pat.tuple [(Pat.var {txt = "u"; loc=(!default_loc)}); Pat.any (); Pat.any (); Pat.any ()]))) (Exp.ident {txt = Lident "u"; loc=(!default_loc)})])] *)

(* let a = Exp.case (Pat.construct {txt = Lident "E"; loc=(!default_loc)} None) (Exp.constant (Pconst_integer ("0", None))) *)
(* let b = Exp.case (Pat.construct {txt = Lident "N"; loc=(!default_loc)} (Some (Pat.tuple [(Pat.var {txt = "u"; loc=(!default_loc)}); Pat.any (); Pat.any (); Pat.any ()]))) (Exp.ident {txt = Lident "u"; loc=(!default_loc)}) *)

(* type constructor = {
  name: string;
  idents: (string * Longident.t Asttypes.loc) list
} *)
let getFunction attribute =
  match attribute with
  | [] -> defaultFunc
  | ({txt = "hash"; _},
     PStr [{ pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident func; _}, _) ; _}])::_ -> func
  | _ -> defaultFunc

let rec getIdAndFunct args =
  (* print_int (List.length args); *)
  match args with
  | [] -> []
  | [{ptyp_desc = Ptyp_constr ({txt = Lident identType; _}, []); _}] -> [{identName = ""; identType = identType; func = defaultFunc}]
  (* [("", defaultFunc)] *)
  | {ptyp_desc = Ptyp_extension({ txt = "hcons"; _}, pstr); _}::{ptyp_desc = Ptyp_constr ({txt = Lident identType; _}, []); ptyp_attributes = attrs; _}::xs ->
    begin
      match pstr with
      | PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_ident {txt = Lident identName; _}; _}, _); _}] -> {identName = identName; identType = identType; func = getFunction attrs} :: getIdAndFunct xs
      (* (id_name, getFunction attrs)::getIdAndFunct xs *)
      | _ -> getIdAndFunct xs
    end
  | _::xs -> {identName = ""; identType = ""; func = defaultFunc} :: getIdAndFunct xs
  (* ("", defaultFunc)::getIdAndFunct xs *)

let rec getConstructors variants =
      match variants with
    (* | {pcd_name = name; pcd_args = Pcstr_tuple []; _}::xs -> {name = name.txt; idents = []}:: getConstructors xs *)
    | [] -> []
    | {pcd_name = name; pcd_args = Pcstr_tuple args; _}::xs -> {consName = name.txt; idents = getIdAndFunct args} :: getConstructors xs
    | _ -> []

(* let rec getCustomType t =
  match t with
  | {pstr_desc = Pstr_type (rec_flag,
    [{ptype_name = name;
      ptype_params = _;
      ptype_cstrs = _;
      ptype_kind = Ptype_variant variants;
      ptype_private = _;
      ptype_manifest = _
      ;_}]); _} ->
      let customType = {typeName = name.txt; constructors = getConstructors variants} in
      print_string "Custom type - {\n";
      print_string ("typeName = "^customType.typeName^"\n");
      print_string ("constructors = "^customType.typeName^"\n");
      printCType customType;
      flush stdout;
      let x = Str.type_ rec_flag [Type.mk ~kind:(Ptype_variant (modifyTypeArg variants)) name] in x
  | _ -> print_string "no match";t *)

(* let modifyType types_str_list = 
  getCustomType (List.hd types_str_list);
  List.map (addInteger) types_str_list *)

let typeT customType = Str.type_ Recursive [Type.mk ~manifest:(Typ.mk (Ptyp_constr ({txt = Lident (!customType.typeName); loc=(!default_loc)}, []))) ({txt = "t"; loc=(!default_loc)})]

(* HASH FUNCTION *)
(* let ae1112 = (Nolabel, Exp.apply (Exp.ident ({ txt = Ldot (Lident "Char", "code"); loc=(!default_loc)})) [(Nolabel, Exp.ident ({txt = Lident "c"; loc=(!default_loc)}))]) *)

(* let ae11112 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "unique"; loc=(!default_loc)})) [(Nolabel, (Exp.ident ({txt = Lident "l"; loc=(!default_loc)})))]) *)

(* let ae11111 = (Nolabel, (Exp.constant (Pconst_integer ("19", None)))) *)

(* let ae1111 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "*"; loc=(!default_loc)})) [ae11111; ae11112]) *)

(* let ae112 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "+"; loc=(!default_loc)})) [ae1111; ae1112]) *)

(* let ae111 = (Nolabel, (Exp.constant (Pconst_integer ("19", None)))) *)

(* let ae12 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "unique"; loc=(!default_loc)})) [(Nolabel, (Exp.ident ({txt = Lident "r"; loc=(!default_loc)})))]) *)

(* let ae11 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "*"; loc=(!default_loc)})) [ae111; ae112]) *)

(* let ae2 = (Nolabel, (Exp.ident ({txt = Lident "max_int"; loc=(!default_loc)}))) *)

(* let ae1 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "+"; loc=(!default_loc)})) [ae11; ae12]) *)
(* let cr = Exp.apply (Exp.ident ({txt = Lident "land"; loc=(!default_loc)})) [ae1; ae2] *)
(* let c1 = Exp.case (Pat.construct {txt = Lident "E"; loc=(!default_loc)} None) (Exp.constant (Pconst_integer ("0", None))) *)

(* let c2 = Exp.case (Pat.construct {txt = Lident "N"; loc=(!default_loc)} (Some (Pat.tuple [Pat.any (); (Pat.var {txt = "l"; loc=(!default_loc)}); (Pat.var {txt = "c"; loc=(!default_loc)}); (Pat.var {txt = "r"; loc=(!default_loc)})]))) cr *)

let applyHash ident n =
let writeEqualCustomID id =
  if String.length ident.identName = 0
  then "u_"^(string_of_int n)
  else id
in
  Exp.apply (Exp.ident ({txt = Lident "*"; loc=(!default_loc)}))
  [(Nolabel, Exp.constant (Pconst_integer ("19", None)));
   (Nolabel,
   (if ident.identType = !customType.typeName
   then Exp.apply (Exp.ident ({txt = Lident "unique"; loc=(!default_loc)}))
   else Exp.apply (Exp.ident ({txt = Ldot (Lident "Hashtbl", "hash"); loc=(!default_loc)})))
  [(Nolabel, (Exp.ident ({txt = Lident (writeEqualCustomID ident.identName); loc=(!default_loc)})))])]

(* let applyCustomHash ident =
  Exp.apply (Exp.ident ({txt = Lident "land"; loc=(!default_loc)}))
    [(Nolabel, Exp.apply (Exp.ident ({txt = Lident "+"; loc=(!default_loc)}))
      [(Nolabel, Exp.apply (Exp.ident func) [(Nolabel, (Exp.ident {txt = Lident (writeCustomID id); loc=(!default_loc)}))]);
       (Nolabel, writeCustomFunc xs)]);
     (Nolabel, (Exp.ident ({txt = Lident "max_int"; loc=(!default_loc)})))] *)

(* let rec writeCustomFunc idents =
  match idents with
  | [] -> Exp.constant (Pconst_integer ("0", None))
  | [x] -> applyHash (fst x)
  | x::xs ->
    let id = fst x in
    let func =
    if snd x == defaultFunc
    then applyHash id
    else Exp.apply (Exp.ident (snd x)) [(Nolabel, (Exp.ident {txt = Lident (writeCustomID id); loc=(!default_loc)}))]
    in
    Exp.apply (Exp.ident ({txt = Lident "land"; loc=(!default_loc)}))
         [(Nolabel, Exp.apply (Exp.ident ({txt = Lident "+"; loc=(!default_loc)}))
          [(Nolabel, func); (Nolabel, writeCustomFunc xs)]);
         (Nolabel, (Exp.ident ({txt = Lident "max_int"; loc=(!default_loc)})))] *)
    (* then 
    else Exp.apply (Exp.ident ({txt = Lident "land"; loc=(!default_loc)}))
         [(Nolabel, Exp.apply (Exp.ident ({txt = Lident "+"; loc=(!default_loc)}))
          [(Nolabel, Exp.apply (Exp.ident func) [(Nolabel, (Exp.ident {txt = Lident (writeCustomID id); loc=(!default_loc)}))]);
           (Nolabel, writeCustomFunc xs)]);
         (Nolabel, (Exp.ident ({txt = Lident "max_int"; loc=(!default_loc)})))] *)
    
    (* Exp.apply (Exp.ident func) [(Nolabel, (Exp.ident {txt = Lident (writeCustomID id); loc=(!default_loc)}))] *)

let writeCustomHashFunc idents =
  let n = 0 in
  let rec writeCustomFunc n idents =
  match idents with
  | [] -> Exp.constant (Pconst_integer ("0", None))
  | [x] -> applyHash x n
  | x::xs ->
    (* let id = fst x in *)
    let func =
    if x.func = defaultFunc
    then applyHash x n
    else Exp.apply (Exp.ident x.func) [(Nolabel, (Exp.ident {txt = Lident (writeCustomID x.identName); loc=(!default_loc)}))]
    in
    Exp.apply (Exp.ident ({txt = Lident "land"; loc=(!default_loc)}))
         [(Nolabel, Exp.apply (Exp.ident ({txt = Lident "+"; loc=(!default_loc)}))
          [(Nolabel, func); (Nolabel, writeCustomFunc (n + 1) xs)]);
         (Nolabel, (Exp.ident ({txt = Lident "max_int"; loc=(!default_loc)})))]
    in writeCustomFunc n idents
  (* if List.length idents == 0 
  then Exp.constant (Pconst_integer ("0", None))
  else  *)
    (* print_int (List.length idents); *)
  (* Exp.apply (Exp.ident (snd (List.hd idents))) [(Nolabel, (Exp.ident {txt = Lident (writeCustomID (fst (List.hd idents))); loc=(!default_loc)}))] *)
  
  let writeHashIdents idents =
  let n = 0 in
  let rec writeEqualIdentsAux n idents =
    let writeEqualCustomID id = if String.length id = 0 then "u_"^(string_of_int n) else id in
    match idents with
    | [] -> []
    | x::xs -> Pat.var {txt = writeEqualCustomID x.identName; loc=(!default_loc)} :: writeEqualIdentsAux (n+1) xs
  in writeEqualIdentsAux n idents

let writeHashCustomIDPat idents =
  if List.length idents = 0 
  then None
  else Some (Pat.tuple (Pat.any () :: (writeHashIdents idents)))

let hashCases constructors =
  let rec hashCasesAux constructors =
    match constructors with
    | [] -> []
    | x::xs -> Exp.case (Pat.construct {txt = Lident x.consName; loc=(!default_loc)} (writeHashCustomIDPat x.idents)) (writeCustomHashFunc x.idents) :: hashCasesAux xs
  in hashCasesAux constructors


let hash customType = Str.value Nonrecursive [Vb.mk (Pat.var {txt = "hash"; loc=(!default_loc)}) (Exp.function_ (hashCases !customType.constructors))]
(*  *)
(* EQUAL FUNCTION *)
(* let d222 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "=="; loc=(!default_loc)})) [(Nolabel, (Exp.ident ({txt = Lident "r1"; loc=(!default_loc)}))); (Nolabel, (Exp.ident ({txt = Lident "r2"; loc=(!default_loc)})))]) *)

(* let d221 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "=="; loc=(!default_loc)})) [(Nolabel, (Exp.ident ({txt = Lident "c1"; loc=(!default_loc)}))); (Nolabel, (Exp.ident ({txt = Lident "c2"; loc=(!default_loc)})))]) *)

(* let d22 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "&&"; loc=(!default_loc)})) [d221; d222]) *)
(* let d21 = (Nolabel, Exp.apply (Exp.ident ({txt = Lident "=="; loc=(!default_loc)})) [(Nolabel, (Exp.ident ({txt = Lident "l1"; loc=(!default_loc)}))); (Nolabel, (Exp.ident ({txt = Lident "l2"; loc=(!default_loc)})))]) *)
(* let d2r = Exp.apply (Exp.ident ({txt = Lident "&&"; loc=(!default_loc)})) [d21; d22] *)

(* let d2 = Exp.case (Pat.tuple [(Pat.construct {txt = Lident "N"; loc=(!default_loc)} (Some (Pat.tuple [Pat.any (); (Pat.var {txt = "l1"; loc=(!default_loc)}); (Pat.var {txt = "c1"; loc=(!default_loc)}); (Pat.var {txt = "r1"; loc=(!default_loc)})]))); (Pat.construct {txt = Lident "N"; loc=(!default_loc)} (Some (Pat.tuple [Pat.any (); (Pat.var {txt = "l2"; loc=(!default_loc)}); (Pat.var {txt = "c2"; loc=(!default_loc)}); (Pat.var {txt = "r2"; loc=(!default_loc)})])))]) d2r *)

(* let d1 = Exp.case (Pat.tuple [Pat.construct {txt = Lident "E"; loc=(!default_loc)} None; Pat.construct {txt = Lident "E"; loc=(!default_loc)} None]) (Exp.construct ({txt = Lident "true"; loc=(!default_loc)}) None) *)

(* let d3 = Exp.case (Pat.any ()) (Exp.construct ({txt = Lident "false"; loc=(!default_loc)}) None) *)



(* let writeEqualCustomID id = if String.length id == 0 then "u" else id *)

(* let d2r = Exp.apply (Exp.ident ({txt = Lident "&&"; loc=(!default_loc)})) [d21; d22] *)

let applyEqual l r = Exp.apply (Exp.ident ({txt = Lident "=="; loc=(!default_loc)})) [(Nolabel, (Exp.ident ({txt = Lident l; loc=(!default_loc)}))); (Nolabel, (Exp.ident ({txt = Lident r; loc=(!default_loc)})))]

let writeEqualArgs idents =
  let n = 0 in
  let rec writeEqualIdentsAux n idents =
    let writeEqualCustomID s id = (s^"_"^id)^(string_of_int n) in
    match idents with
    | [] -> Exp.construct ({txt = Lident "true"; loc=(!default_loc)}) None
    | x::[] -> applyEqual (writeEqualCustomID "l" x.identName) (writeEqualCustomID "r" x.identName)
    | x::xs -> Exp.apply (Exp.ident ({txt = Lident "&&"; loc=(!default_loc)})) [(Nolabel, applyEqual (writeEqualCustomID "l" x.identName) (writeEqualCustomID "r" x.identName)); (Nolabel, writeEqualIdentsAux (n + 1) xs)]
    (* | _ -> applyEqual (writeEqualCustomID "l" "l") (writeEqualCustomID "r" "r") *)
  in writeEqualIdentsAux n idents

(* let writeCustomIDExpr idents =
  if List.length idents == 0 
  then Exp.construct ({txt = Lident "true"; loc=(!default_loc)}) None
  else 
  writeEqualArgs idents *)
  (* (Exp.ident {txt = Lident (writeCustomID (fst (List.hd idents))); loc=(!default_loc)}) *)

let writeEqualIdents s idents =
  let n = 0 in
  let rec writeEqualIdentsAux n s idents =
    let writeEqualCustomID id = (s^"_"^id)^(string_of_int n) in
    match idents with
    | [] -> []
    | x::xs -> Pat.var {txt = writeEqualCustomID x.identName; loc=(!default_loc)} :: writeEqualIdentsAux (n + 1) s xs
  in writeEqualIdentsAux n s idents

let writeEqualCustomIDPat s idents =
  if List.length idents = 0 
  then None
  else Some (Pat.tuple (Pat.any () :: (writeEqualIdents s idents)))

let equalCases constructors =
  (* print_string "Const length = ";
  print_int (List.length constructors);
  print_newline (); *)
  let rec equalCasesAux constructors =
    match constructors with
    | [] ->  []
    | x::xs -> Exp.case (Pat.tuple [(Pat.construct {txt = Lident x.consName; loc=(!default_loc)} (writeEqualCustomIDPat "l" x.idents)); (Pat.construct {txt = Lident x.consName; loc=(!default_loc)} (writeEqualCustomIDPat "r" x.idents))]) (writeEqualArgs x.idents) :: equalCasesAux xs
  in equalCasesAux constructors

(* let d1 = Exp.case (Pat.tuple [(Pat.construct {txt = Lident x.name; loc=(!default_loc)} (writeEqualCustomIDPat "l" x.idents)); (Pat.construct {txt = Lident x.name; loc=(!default_loc)} (writeEqualCustomIDPat "r" x.idents))]) (Exp.construct ({txt = Lident "true"; loc=(!default_loc)}) None) *)

(* let d = Exp.match_ (Exp.tuple [Exp.ident {txt = Lident "t1"; loc=(!default_loc)}; Exp.ident  {txt = Lident "t2"; loc=(!default_loc)}]) (equalCases !customType.constructors) *)
 (* [d1; d2; d3] *)

(* let c = Exp.fun_ Nolabel None (Pat.var {txt = "t2"; loc=(!default_loc)}) d *)
(* let b = Exp.fun_ Nolabel None (Pat.var {txt = "t1"; loc=(!default_loc)}) c *)
(* let a = Vb.mk (Pat.var {txt = "equal"; loc=(!default_loc)}) b *)
let equal customType = Str.value Nonrecursive [
  Vb.mk (Pat.var {txt = "equal"; loc=(!default_loc)})
    (Exp.fun_ Nolabel None (Pat.var {txt = "t1"; loc=(!default_loc)})
    (Exp.fun_ Nolabel None (Pat.var {txt = "t2"; loc=(!default_loc)}) (Exp.match_ (Exp.tuple [Exp.ident {txt = Lident "t1"; loc=(!default_loc)};
     Exp.ident {txt = Lident "t2"; loc=(!default_loc)}]) (equalCases !customType.constructors @ [Exp.case (Pat.any ()) (Exp.construct ({txt = Lident "false"; loc=(!default_loc)}) None)]))))
]
(*  *)

(* Module *)
let module_ customType = Str.module_ (Mb.mk ({txt = "X"; loc=(!default_loc)}) (Mod.structure [typeT customType; hash customType; equal customType]))
(*  *)

(* Instanciate WeakHashtable *)
let weakHashtable = Str.module_ (Mb.mk ({txt = "W"; loc=(!default_loc)}) (Mod.apply (Mod.ident { txt = Ldot (Lident "Weak", "Make"); loc=(!default_loc)}) (Mod.ident {txt = Lident "X"; loc=(!default_loc)})))
(*  *)

(* Create table *)
let nodes = Str.value Nonrecursive [Vb.mk (Pat.var {txt = "nodes"; loc=(!default_loc)}) (Exp.apply (Exp.ident ({ txt = Ldot (Lident "W", "create"); loc=(!default_loc)})) [(Nolabel, Exp.constant (Pconst_integer ("5003", None)))])]
(*  *)

(* Empty function *)
let empty = Str.value Nonrecursive [Vb.mk (Pat.var {txt = "empty"; loc=(!default_loc)}) (Exp.construct ({txt = Lident "E"; loc=(!default_loc)}) None)]
(*  *)

(* Node function *)
let n2111221then = (Exp.apply (Exp.ident ({ txt = Lident "incr"; loc=(!default_loc)})) [(Nolabel, Exp.ident {txt = Lident "cpt"; loc=(!default_loc)})])
let n2111221if = (Exp.apply (Exp.ident ({ txt = Lident "=="; loc=(!default_loc)})) [(Nolabel, Exp.ident {txt = Lident "n"; loc=(!default_loc)});
(Nolabel, Exp.ident {txt = Lident "n0"; loc=(!default_loc)})])
let n2111221 = Exp.ifthenelse n2111221if n2111221then None

let n211122 = Exp.sequence n2111221 (Exp.ident {txt = Lident "n"; loc=(!default_loc)})

let n211121 = (Exp.apply (Exp.ident ({ txt = Ldot (Lident "W", "merge"); loc=(!default_loc)})) [(Nolabel, Exp.ident {txt = Lident "nodes"; loc=(!default_loc)});
(Nolabel, Exp.ident {txt = Lident "n0"; loc=(!default_loc)})])

let n21112 = Exp.let_ Nonrecursive [Vb.mk (Pat.var {txt = "n"; loc=(!default_loc)}) n211121] n211122

let n211111 = (Exp.tuple [
  Exp.apply (Exp.ident {txt = Lident "!"; loc=(!default_loc)}) [(Nolabel, Exp.ident {txt = Lident "cpt"; loc=(!default_loc)})];
  Exp.ident {txt = Lident "l"; loc=(!default_loc)};
  Exp.ident {txt = Lident "c"; loc=(!default_loc)};
  Exp.ident {txt = Lident "r"; loc=(!default_loc)}])
let n21111 = (Exp.construct ({txt = Lident "N"; loc=(!default_loc)}) (Some n211111)) 

let n2111 = Exp.let_ Nonrecursive [Vb.mk (Pat.var {txt = "n0"; loc=(!default_loc)}) n21111] n21112 
let n211 = Exp.fun_ Nolabel None (Pat.var {txt = "r"; loc=(!default_loc)}) n2111

let n21 = Exp.fun_ Nolabel None (Pat.var {txt = "c"; loc=(!default_loc)}) n211

let n2 = Exp.fun_ Nolabel None (Pat.var {txt = "l"; loc=(!default_loc)}) n21

let n1 = Exp.let_ Nonrecursive [Vb.mk (Pat.var {txt = "cpt"; loc=(!default_loc)}) (Exp.apply (Exp.ident ({ txt = Lident "ref"; loc=(!default_loc)})) [(Nolabel, Exp.constant (Pconst_integer ("1", None)))])] n2

let node = Str.value Nonrecursive [Vb.mk (Pat.var {txt = "node"; loc=(!default_loc)}) n1]
(*  *)
let rec str_item_defunc_mapper mapper str = 
   begin match str with
      | { pstr_desc =
          Pstr_extension (({ txt = "hashcons"; loc }, pstr), _attributes); _} -> 
          begin 
            match pstr with
            | PStr [{ pstr_desc =
                    Pstr_value (rec_flag,
                    l); _}] -> Str.value rec_flag (createFunc l)
            | PStr [{pstr_desc = Pstr_type (rec_flag,
    [{ptype_name = name;
      ptype_params = _;
      ptype_cstrs = _;
      ptype_kind = Ptype_variant variants;
      ptype_private = _;
      ptype_manifest = _
      ;_}]); _}] -> Str.type_ rec_flag [Type.mk ~kind:(Ptype_variant (modifyTypeArg variants)) name]
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
          end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
      end

let rec str_defunc_mapper mapper str_list =
  let types_str_list, other_str_list = separateTypeStr str_list in
  (* Add integer to types *)
(*   
  let newTypes = modifyType types_str_list in
  (* Create table *)
  let table = Str.value Nonrecursive [
                Vb.mk (Pat.var {txt = "table"; loc=(!default_loc)})
                (Exp.apply (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "create"); loc=(!default_loc)}))
                [(Nolabel, Exp.constant (Pconst_integer ("251", None)))])
              ]
  in *)

  (* Create hashcons *)
  let hashcons = Str.value Nonrecursive [
                  Vb.mk (Pat.var {txt = "hashcons"; loc=(!default_loc)})
                  (Exp.fun_ Nolabel None (Pat.var {txt = "x"; loc=(!default_loc)})
                  (Exp.try_ (Exp.apply
                  (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "find"); loc=(!default_loc)}))
                  [(Nolabel, Exp.ident {txt = Lident "table"; loc=(!default_loc)});
                   (Nolabel, Exp.ident {txt = Lident "x"; loc=(!default_loc)})])
                   [({
                     pc_lhs = Pat.construct {txt = Lident "Not_found"; loc=(!default_loc)}
 None; 
                     pc_guard = None;
  	                 pc_rhs = ((Exp.sequence
                     (Exp.apply
                     (Exp.ident ({ txt = Ldot (Lident "Hashtbl", "add"); loc=(!default_loc)}))
                      [(Nolabel, Exp.ident {txt = Lident "table"; loc=(!default_loc)});
                      (Nolabel, Exp.ident {txt = Lident "x"; loc=(!default_loc)});
                      (Nolabel, Exp.ident {txt = Lident "x"; loc=(!default_loc)})])
                      (Exp.ident {txt = Lident "x"; loc=(!default_loc)})))})]))
                ]
  in
  let res = List.fold_left (fun acc str ->
  begin match str with
  | {pstr_desc = Pstr_extension (({ txt = "hashcons"; _ }, pstr), _attributes); _} ->
      begin 
      match pstr with
      | PStr [{ pstr_desc =
              Pstr_value (_,
              _); _}] ->
(* let exp = str_item_defunc_mapper mapper str in
                exp:: *)
                acc
      | PStr [{pstr_desc = Pstr_type (rec_flag,
    [{ptype_name = name;
      ptype_params = _;
      ptype_cstrs = _;
      ptype_kind = Ptype_variant variants;
      ptype_private = _;
      ptype_manifest = _
      ;_}]); _}] ->  
      (* let type_ = str_item_defunc_mapper mapper str in
      type_:: *)
      customType := {typeName = name.txt; constructors = getConstructors variants};
      (* let customType = {typeName = name.txt; constructors = getConstructors variants} in *)
      (* printCType !customType; *)
      (* type_name := name.txt; *)
      let newType = Str.type_ rec_flag [Type.mk ~kind:(Ptype_variant (modifyTypeArg variants)) name] in
      let unique = unique !customType.constructors in
      let module_ = module_ customType in
      List.rev (newType::unique::module_::weakHashtable::nodes::
      (* empty::node:: *)
      acc)
       | _ -> 
       (* (str_item_defunc_mapper mapper str):: *)
       acc                   
       end
  | x -> (default_mapper.structure_item mapper x)::acc
  end) [] other_str_list in (List.rev res)
    (* List.rev (node::empty::nodes::weakHashtable::module_::unique::res) *)
(* (res@hashcons::table::node::empty::nodes::weakHashtable::module_::unique::newTypes) *)
(* 
let rec str_item_print_mapper mapper str = 
   begin match str with
      | { pstr_desc =
          Pstr_extension (({ txt = "print"; _ }, _), _); _} ->
          print_string "%print found \n";
          print_string "\nFree vars: ";
          Hashtbl.iter print_hashtbl_list free_vars;
          print_string "\nVars: ";
          Hashtbl.iter print_hashtbl vars;
          print_string "\nAttributes: ";
          print_queue attributes;
          Str.eval (Exp.constant (Pconst_string ("", None)));
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
      end *)

let defunc_mapper _argv =
  { 
    default_mapper with
    structure = str_defunc_mapper;
    structure_item = str_item_defunc_mapper
  }
(* 
let print_mapper _argv =
  { 
    default_mapper with
    structure_item = str_item_print_mapper
  } *)

let () = register "defun" defunc_mapper;
         (* register "print" print_mapper *)