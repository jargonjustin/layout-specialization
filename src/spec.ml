
open Ast
open Util

let binops = [("+", ( +. )); ("-", ( -. )); ("*", ( *. )); ("/", ( /. ))]

(** Evaluates instructions in a stream *)
let interpret instructions =
   let rec eval context expr =
      let Instance (_, fields, children, attributes) = context in
      match expr with
       | Literal x -> x
       | FieldRef field -> (Hashtbl.find fields field).value
       | AttrRef (ChildAttrRef (child, attr)) ->
            let Instance (_, _, _, child_attrs) = Hashtbl.find children child in
            Hashtbl.find child_attrs attr
       | AttrRef (SelfAttrRef attr) -> Hashtbl.find attributes attr
       | BinOp (op, lhs, rhs) -> (List.assoc op binops) (eval context lhs) (eval context rhs) in
   let exec instruction =
      let Inst (Instance (_, _, _, attributes), attr, context, expr) = instruction in
      Hashtbl.replace attributes attr (eval context expr) in
   Stream.iter exec instructions

(** Partially evaluates instructions in a stream, returning the new stream *)
let specialize =
   (** Evaluates as much of the expression as possible *)
   let rec partial_eval context expr =
      let Instance (_, fields, children, attributes) = context in
      match expr with
       | Literal x -> Right x
       | FieldRef name ->
            let field = Hashtbl.find fields name in
            if field.dynamic then Left expr else Right field.value
       | AttrRef (ChildAttrRef (child, attr)) ->
            let Instance (_, _, _, child_attrs) = Hashtbl.find children child in
            (try Right (Hashtbl.find child_attrs attr) with Not_found -> Left expr)
       | AttrRef (SelfAttrRef attr) -> (try Right (Hashtbl.find attributes attr) with Not_found -> Left expr)
       | BinOp (op, lhs, rhs) ->
            match (partial_eval context lhs, partial_eval context rhs) with
             | (Left plhs, Left prhs) -> Left (BinOp (op, plhs, prhs))
             | (Right elhs, Left prhs) -> Left (BinOp (op, Literal elhs, prhs))
             | (Left plhs, Right erhs) -> Left (BinOp (op, plhs, Literal erhs))
             | (Right elhs, Right erhs) -> Right ((List.assoc op binops) elhs erhs) in
   let partial_exec = function
    | Inst (Instance (_, _, _, attributes) as data, attr, context, expr) ->
         match partial_eval context expr with
          | Left pexpr -> Some (Inst (data, attr, context, pexpr))
          | Right result -> Hashtbl.replace attributes attr result; None
   in StreamExt.filter_map partial_exec

(** Clears the attributes of all nodes in an instruction stream *)
let reset =
   Stream.iter (fun inst ->
      let Inst (Instance (_, _, _, attributes), _, _, _) = inst in Hashtbl.clear attributes)

(** Determines the relationship between an attribute being set and its context node *)
let reconstruct data attr context =
   if data == context then
      SelfAttrRef attr
   else
      let Instance (_, _, children, _) = context in
      let (child, _) = HashtblExt.search children (fun _ klass -> data == klass) in
      ChildAttrRef (child, attr)

(** Dumps an instruction stream for debugging *)
let examine out =
   (** Dumps an instrction to stderr *)
   let print_reconstruction instruction =
      let Inst (data, attr, context, expr) = instruction in
      let Instance (klass, _, _, _) = context in
      Printf.fprintf out "   %-10s%-20s%s\n" klass (string_of_attr (reconstruct data attr context)) (string_of_expr expr) in
   Stream.iter print_reconstruction
