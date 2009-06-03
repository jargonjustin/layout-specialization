
open Ast
open Util

exception Parse_error of string

(** Parses a data file containing instances of the provided classes *)
let parse_stream klasses contracts stream =
   (** Looks up a class definition by name *)
   let lookup_class name =
      try List.find (fun klass -> klass.name = name) klasses with Not_found -> raise (Parse_error ("Class " ^ name ^ " is not defined")) in
   (* The recursive-decent parser *)
   let lexer = Genlex.make_lexer ["{"; "}"; ":"; ";"] in
   let rec parse_data = parser
      | [< 'Genlex.Kwd "{"; 'Genlex.Ident name; data = parse_body (lookup_class name); 'Genlex.Kwd "}" >] ->
         let klass = lookup_class name in
         let Instance (_, fields, children, _) = data in
         ListExt.for_each klass.fields (fun field ->
            if not (Hashtbl.mem fields field) then
               raise (Parse_error ("Instances of " ^ klass.name ^ " must define field '" ^ field ^ "'")));
         ListExt.for_each klass.children (fun (child, child_type) ->
            try
               let Instance (child_klass, _, _, _) = Hashtbl.find children child in
               if not (child_klass = child_type || List.mem child_type (lookup_class child_klass).interfaces) then
                  raise (Parse_error ("Instances of " ^ klass.name ^ " must define a child '" ^ child ^ "' of type " ^ child_type ^ " (got " ^ child_klass ^ ")"))
            with
               Not_found -> raise (Parse_error ("Instances of " ^ klass.name ^ " must define child '" ^ child ^ "'")));
         data
   and parse_body klass = parser
      | [< entry = parse_entry; data = parse_body klass >] ->
         let Instance (_, fields, children, _) = data in
         (match entry with
            | (field, `Field value) ->
               if not (List.mem field klass.fields) then
                  raise (Parse_error ("Class " ^ klass.name ^ " does not define a field named '" ^ field ^ "'"));
               if (Hashtbl.mem fields field) then
                  raise (Parse_error ("An instance of class " ^ klass.name ^ " defines field '" ^ field ^ "' multiple times"));
               Hashtbl.add fields field value; data
            | (child, `Child value) ->
               if not (List.mem_assoc child klass.children) then
                  raise (Parse_error ("Class " ^ klass.name ^ " does not define a child named '" ^ child ^ "'"));
               if (Hashtbl.mem children child) then
                  raise (Parse_error ("An instance of class " ^ klass.name ^ " defines child '" ^ child ^ "' multiple times"));
               Hashtbl.add children child value; data)
      | [< >] ->
         Instance (klass.name, Hashtbl.create 2, Hashtbl.create 2, Hashtbl.create 4)
   and parse_entry = parser
      | [< 'Genlex.Ident entry; 'Genlex.Kwd ":"; value = parse_dynamic; 'Genlex.Kwd ";" >] -> (entry, value)
   and parse_dynamic = parser
      | [< 'Genlex.Ident "dynamic"; value=parse_value true >] -> value
      | [< value=parse_value false >] -> value
   and parse_value is_dynamic = parser
      | [< 'Genlex.Int x >] -> `Field {dynamic=is_dynamic; value=(float x)}
      | [< 'Genlex.Float x >] -> `Field {dynamic=is_dynamic; value=x}
      | [< data = parse_data >] -> `Child data
   in try
      (* Parse the tree *)
      let tree = parse_data (lexer stream) in
      (* Make sure the root node doesn't require attributes be set *)
      let () =
         let Instance (klass, _, _, _) = tree in
         let (_, inherited) = StringMap.find klass contracts in
         if not (StringSet.is_empty inherited) then
            raise (Parse_error ("The root node in a data tree must not have any inherited attributes (class " ^ klass ^ " is not suitable)")) in
      tree
   with
      | Stream.Error _ -> raise (Parse_error "data syntax error")

let parse_channel klasses contracts channel =
   parse_stream klasses contracts (Stream.of_channel channel)

let parse_file klasses contracts filename =
   parse_channel klasses contracts (open_in filename)

(** Creates an instruction stream to evaluate an attribute grammar *)
let rec compile klasses orderings data = 
   let Instance (name, _, children, _) = data in
   let klass = List.find (fun klass -> klass.name = name) klasses in
   (** Expands an evaluation step into stream of instructions which will evaluate that step *)
   let expand = function
    | EvalAttr (ChildAttrRef (child, attr) as ref) ->
         [< 'Inst (Hashtbl.find children child, attr, data, List.assoc ref klass.definitions) >]
    | EvalAttr (SelfAttrRef attr as ref) ->
         [< 'Inst (data, attr, data, List.assoc ref klass.definitions) >]
    | EvalChild child ->
         compile klasses orderings (Hashtbl.find children child) in
   
   StreamExt.map expand (StringMap.find name orderings)

(** Pretty-prints a data instance for debugging *)
let pretty_print out data =
   let print_indent indent = for i = 0 to indent - 1 do output_string out "   " done in
   let print_members indent kf vf =
      Hashtbl.iter (fun k v ->
         print_indent (indent + 1);
         output_string out (kf k);
         output_string out ": ";
         vf v;
         output_string out ";\n") in
   let print_field field =
      if field.dynamic then
         output_string out "dynamic ";
      output_string out (string_of_float field.value) in
   let rec pp indent data =
      let Instance (klass, fields, children, attributes) = data in
      output_string out ("{" ^ klass ^ "\n");
      print_members indent (fun attr -> "@" ^ attr) (fun x -> output_string out (string_of_float x)) attributes;
      print_members indent identity print_field fields;
      print_members indent identity (pp (indent + 1)) children;
      print_indent indent;
      output_string out "}" in
   pp 0 data; output_string out "\n"; flush out

(** Prints a GraphViz formatted graph showing how attributes depend upon each other and dynamicism flows through the tree *)
let graph out klasses contracts orderings tree =
   (** Resolves an attribute reference to a (node, attribute) pair given a context and a reference *)
   let resolve_reference context = function
    | ChildAttrRef (child_name, attr) ->
         let Instance (klass, _, children, _) = context in
         let child_node = try Hashtbl.find children child_name
            with Not_found -> failwith ("Instance of " ^ klass ^ " missing child named " ^ child_name) in
         (child_node, attr)
    | SelfAttrRef (attr) ->
         (context, attr) in
   (** Looks up a class by name *)
   let lookup_class name =
      List.find (fun klass -> klass.name = name) klasses in
   (* Bound attribute tables *)
   (* For some reason, the polymorphic Hashtbl doesn't correctly handle instance data, so we use our own *)
   let module LabelHashedType =
      struct
         type t = data * string
         let equal (n1, a1) (n2, a2) = (n1 == n2) && (a1 = a2)
         let hash (Instance (klass, fields, children, _), attr) =
            (Hashtbl.hash klass) lxor (Hashtbl.hash fields) lxor (Hashtbl.hash children) lxor (Hashtbl.hash attr)
      end in
   let module Labeltbl = Hashtbl.Make(LabelHashedType) in
   (** Checks if an attribute definition is dynamic in a particular context *)
   let is_dynamic labels context definition =
      let has_dynamic_field definition context =
         let Instance (_, fields, _, _) = context in
         let is_field_dynamic field = (Hashtbl.find fields field).dynamic in
         StringSet.exists is_field_dynamic (Grammar.field_dependencies definition) in
      let has_dynamic_attribute definition context  =
         let is_attribute_dynamic attr_ref = fst (snd (Labeltbl.find labels (resolve_reference context attr_ref))) in
         AttrRefSet.exists is_attribute_dynamic (Grammar.attribute_dependencies definition) in
      let is_dynamic_root = has_dynamic_field definition context in
      (is_dynamic_root || has_dynamic_attribute definition context, is_dynamic_root) in
   (** Recursively generates a table labelling each (node, attribute) -> (node id, (is dynamic, is dynamic root)) *)
   let rec label labels counter context =
      let Instance (class_name, _, children, _) = context in
      let klass = lookup_class class_name in
      ListExt.for_each (StringMap.find class_name orderings) (function
       | EvalAttr (attr_ref) ->
            let attr_label = ((incr counter; !counter), is_dynamic labels context (List.assoc attr_ref klass.definitions)) in
            Labeltbl.add labels (resolve_reference context attr_ref) attr_label
       | EvalChild (child) -> ignore (label labels counter (Hashtbl.find children child)));
      labels in
   (** Recursively outputs clusters of attributes grouped by node *)
   let rec cluster labels counter node =
      let Instance (class_name, _, children, _) = node in
      let attributes =
         let (synthesized, inherited) = StringMap.find class_name contracts in
         StringSet.union synthesized inherited in
      Printf.fprintf out "\tsubgraph cluster%i {\n" (incr counter; !counter);
      Printf.fprintf out "\t\tlabel = \"%s\";\n" class_name;
      StringSet.iter (fun attr ->
         let (node_id, (node_is_dynamic, node_is_dynamic_root)) = Labeltbl.find labels (node, attr) in
         Printf.fprintf out "\t\tn%i [label=\"@%s\",color=%s,fillcolor=%s,style=filled];\n"
            node_id attr (if node_is_dynamic then "red" else "black") (if node_is_dynamic_root then "yellow" else "none")) attributes;
      Printf.fprintf out "\t}\n\n";
      Hashtbl.iter (fun _ child -> cluster labels counter child) children in
   (** Sets of edges to prevent duplication *)
   let module EdgeSet = Set.Make(struct type t = int * int let compare = compare end) in
   (** Recursively outputs dependency edges in the tree *)
   let rec connect labels emitted node =
      let emit_edge source_attr_ref target_attr_ref =
         let (source_id, (source_dynamic, _)) = Labeltbl.find labels (resolve_reference node source_attr_ref) in
         let (target_id, _) = Labeltbl.find labels (resolve_reference node target_attr_ref) in
         if not (EdgeSet.mem (source_id, target_id) !emitted) then
            (Printf.fprintf out "\t\tn%i -> n%i [color=%s];\n" source_id target_id (if source_dynamic then "red" else "black");
             emitted := EdgeSet.add (source_id, target_id) !emitted) in
      let Instance (class_name, _, children, _) = node in
      ListExt.for_each (lookup_class class_name).definitions (fun (target_attr_ref, definition) ->
         AttrRefSet.iter (fun source_attr_ref -> emit_edge source_attr_ref target_attr_ref) (Grammar.attribute_dependencies definition));
      Hashtbl.iter (fun _ child -> connect labels emitted child) children in
   
   (* Output the dependency graph *)
   Printf.fprintf out "digraph treedeps {\n";
   Printf.fprintf out "\tranksep = \"1.2 equally\";\n";
   let labels = label (Labeltbl.create 16) (ref 0) tree in
   cluster labels (ref 0) tree;
   connect labels (ref EdgeSet.empty) tree;
   Printf.fprintf out "}\n"
