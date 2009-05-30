
open Ast
open Util

exception Parse_error of string

(** Parses a data file containing instances of the provided classes *)
let parse_stream klasses stream =
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
      parse_data (lexer stream)
   with
      | Stream.Error _ -> raise (Parse_error "data syntax error")

let parse_channel klasses channel =
   parse_stream klasses (Stream.of_channel channel)

let parse_file klasses filename =
   parse_channel klasses (open_in filename)

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
