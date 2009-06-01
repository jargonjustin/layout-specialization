
open Ast
open Util

exception Invalid_grammar of string

let invalid msg =
   raise (Invalid_grammar msg)

(* TODO: This should really be a macro *)
let illegal cond msg =
   if cond then invalid msg

let parse_channel channel =
   let lexbuf = Lexing.from_channel channel in
   try
      Parser.grammar Lexer.grammar lexbuf
   with
      Parsing.Parse_error -> invalid ("grammar syntax error")

let parse_file filename =
   parse_channel (open_in filename)

(** Organizes attribute definitions by (synthesized attr -> def, child -> inherited attr -> def) *)
let definitions klass =
   let fold_children inherited_defs (child, _) =
      illegal (StringMap.mem child inherited_defs)
              ("Multiple declarations of child " ^ child ^ " in class " ^ klass.name);
      StringMap.add child StringMap.empty inherited_defs in
   let fold_defs (synthesized, inherited) def =
      match def with
       | (SelfAttrRef attr, expr) ->
          illegal (StringMap.mem attr synthesized)
                  ("Multiple definitions of @" ^ attr ^ " in class " ^ klass.name);
          (StringMap.add attr expr synthesized, inherited)
       | (ChildAttrRef (child, attr), expr) ->
          let folded_attrs = try StringMap.find child inherited
            with Not_found -> invalid ("Class " ^ klass.name ^ " defines an attribute " ^ child ^ "@" ^ attr ^ " but does not declare the child " ^ child) in
          illegal (StringMap.mem attr folded_attrs)
                  ("Multiple definitions of " ^ child ^ "@" ^ attr ^ " in class " ^ klass.name);
          (synthesized, StringMap.add child (StringMap.add attr expr folded_attrs) inherited) in
   let empty_inherited_defs = List.fold_left fold_children StringMap.empty klass.children in
   List.fold_left fold_defs (StringMap.empty, empty_inherited_defs) klass.definitions

(** Calculates the contracts defined by interfaces and classes in a grammar.
 *  A contract is the non-overlaping sets of inherited and synthesized attributes declared by an interface.
 *  For a class, the contracts of its interfaces are merged.
 *  This method returns a map of class or interface name -> contract.
 *)
let contracts ifaces klasses =
      
   (** The empty contract *)
   let empty_contract = (StringSet.empty, StringSet.empty) in
   (* Functions for adding attributes to contracts *)
   let overlap_message attr = "The attribute @" ^ attr ^ " is declared to be both inherited and synthesized" in
   let add_inherited attr (synthesized, inherited) =
      illegal (StringSet.mem attr synthesized) (overlap_message attr);
      (synthesized, StringSet.add attr inherited) in
   let add_synthesized attr (synthesized, inherited) =
      illegal (StringSet.mem attr inherited) (overlap_message attr);
      (StringSet.add attr synthesized, inherited) in
   (** Calculates the union of two contracts *)
   let union_contract (syna, inha) (synb, inhb) =
      let synu = StringSet.union syna synb in
      let inhu = StringSet.union inha inhb in
      StringSet.iter (fun attr -> invalid (overlap_message attr)) (StringSet.inter synu inhu);
      (synu, inhu) in
   
   (** Calculates the contract for an interface *)
   let interface_contract attrs =
      let fold_decls contract = function
       | SynthesizeDecl attr -> add_synthesized attr contract
       | InheritDecl attr -> add_inherited attr contract
      in List.fold_left fold_decls empty_contract attrs in
   (** Calculates the contract for a class by merging the contracts of its interfaces *)
   let klass_contract interface_contracts klass =
      let lookup_interface iface =
         try
            StringMap.find iface interface_contracts
         with
            Not_found -> invalid ("Class " ^ klass.name ^ " extends an undefined interface " ^ iface) in
      List.fold_left union_contract empty_contract (List.map lookup_interface klass.interfaces) in
   
   (* Collect the map of all interface contracts *)
   let fold_interfaces interface_contracts (Interface (name, attrs)) =
      illegal (StringMap.mem name interface_contracts)
              ("Multiple declarations of interfaces named " ^ name);
      StringMap.add name (interface_contract attrs) interface_contracts in
   let fold_klasses interface_contracts contracts klass =
      illegal (StringMap.mem klass.name contracts)
              ("Multiple definitions of entities named " ^ klass.name);
      StringMap.add klass.name (klass_contract interface_contracts klass) contracts in
   let interface_contracts = List.fold_left fold_interfaces StringMap.empty ifaces in
   List.fold_left (fold_klasses interface_contracts) interface_contracts klasses

(** Calculates the set of attributes an expression depends upon *)
let rec attribute_dependencies = function
 | Literal _ | FieldRef _ -> AttrRefSet.empty
 | AttrRef attr_ref -> AttrRefSet.add attr_ref AttrRefSet.empty
 | BinOp (_, lhs, rhs) -> AttrRefSet.union (attribute_dependencies lhs) (attribute_dependencies rhs)

(** Calculates the set of fields an expression depends upon *)
let rec field_dependencies = function
 | Literal _ | AttrRef _ -> StringSet.empty
 | FieldRef field -> StringSet.add field StringSet.empty
 | BinOp (_, lhs, rhs) -> StringSet.union (field_dependencies lhs) (field_dependencies rhs)

(** Checks that a class conforms to its declared type, raising an Invalid_grammar exception if not *)
let typecheck contracts klass =
   (** Returns the key set of a StringMap *)
   let keys map = StringMap.fold (fun key _ acc -> StringSet.add key acc) map StringSet.empty in   
   
   (** Calculates the set of attribute references that are valid for a given child *)
   let valid_attributes_for_child (child, child_type) =
      let attr_ref_set_of_string_set strings =
         let fold_strings attr acc = AttrRefSet.add (ChildAttrRef (child, attr)) acc in
         StringSet.fold fold_strings strings AttrRefSet.empty in
      let (synthesized, inherited) = StringMap.find child_type contracts in
      AttrRefSet.union (attr_ref_set_of_string_set synthesized) (attr_ref_set_of_string_set inherited) in
   
   (** Checks that a declaration set matches a definition set *)
   let check ?(child="") decl defn =
      StringSet.iter (fun attr -> invalid ("Class " ^ klass.name ^ " declares but does not define " ^ child ^ "@" ^ attr))
                     (StringSet.diff decl defn);
      StringSet.iter (fun attr -> invalid ("Class " ^ klass.name ^ " defines but does not declare " ^ child ^ "@" ^ attr))
                     (StringSet.diff defn decl) in
   (** Checks that a class defines all of the inherited attributes of a child *)
   let check_child (child, child_type) inherited_definitions =
      let (_, inherited_declarations) =
         try
            StringMap.find child_type contracts
         with
            Not_found -> invalid ("Class " ^ klass.name ^ " declares a child " ^ child_type ^ " of non-existant type " ^ child_type) in
      check ~child:child inherited_declarations (keys (StringMap.find child inherited_definitions)) in
   
   (* Calculate the declaration and definition sets of the class *)
   let (decl_synthesized, decl_inherited) = StringMap.find klass.name contracts in
   let (defn_synthesized, defn_inherited) = definitions klass in
   
   (* Check that the class defines all of it's synthesized attributes *)
   check decl_synthesized (keys defn_synthesized);
   (* Check that the class children definitions are complete *)
   ListExt.for_each klass.children (fun child_decl -> check_child child_decl defn_inherited);
   (* Check that attribute definitions refer only to valid attributes and fields *)
   let valid_fields = List.fold_left (flip StringSet.add) StringSet.empty klass.fields in
   let valid_attrs =
      let valid_self_attrs = StringSet.fold (fun attr acc -> AttrRefSet.add (SelfAttrRef attr) acc) decl_inherited AttrRefSet.empty in
      List.fold_left AttrRefSet.union valid_self_attrs (List.map valid_attributes_for_child klass.children) in
   ListExt.for_each klass.definitions (fun (attr, expr) ->
      StringSet.iter  (fun field -> invalid ("The definition of " ^ string_of_attr attr ^ " in " ^ klass.name ^ " references non-existant field " ^ field))
                      (StringSet.diff (field_dependencies expr) valid_fields);
      AttrRefSet.iter (fun attr_ref -> invalid ("The definition of " ^ string_of_attr attr ^ " in " ^ klass.name ^ " references non-existant attribute " ^ string_of_attr attr_ref))
                      (AttrRefSet.diff (attribute_dependencies expr) valid_attrs))

(** Calculates a dependency graph for the evaluation order of attributes and child nodes *)
let dependency_graph contracts klass =
   let vertices =
      let children = List.map (fun (child, _) -> EvalChild child) klass.children in
      let attributes = 
         let attributes_for_child (child, child_type) =
            let fold_attribute attr acc = EvalAttr (ChildAttrRef (child, attr)) :: acc in
            let (synthesized, inherited) = StringMap.find child_type contracts in
            List.append (StringSet.fold fold_attribute synthesized [])
                        (StringSet.fold fold_attribute inherited []) in
         let child_attributes = ListExt.concat_map attributes_for_child klass.children in
         let self_attributes =
            let fold_self_attributes attr acc = EvalAttr (SelfAttrRef attr) :: acc in
            let (synthesized, inherited) = StringMap.find klass.name contracts in
            StringSet.fold fold_self_attributes (StringSet.union synthesized inherited) [] in
         List.append self_attributes child_attributes in
      List.append children attributes in
   let edges =
      (** Contractual edges enforce that the synthesized attributes of child nodes can only be evaluation after
          the child node, and that the inherited attributes of a child node must be evaluated before that child
          node. That is, inherited attributes -> child node -> synthesized attributes. *)
      let contractual_edges (child, child_type) =
         let eval_child = EvalChild child in
         let (synthesized, inherited) = StringMap.find child_type contracts in
         List.append (StringSet.fold (fun attr acc -> (eval_child, EvalAttr (ChildAttrRef (child, attr))) :: acc) synthesized [])
                     (StringSet.fold (fun attr acc -> (EvalAttr (ChildAttrRef (child, attr)), eval_child) :: acc) inherited []) in
      (** An attribute depends upon all of the attributes in its definition *)
      let definition_edges (dependent_attr_ref, expr) =
         let dependent = EvalAttr dependent_attr_ref in
         let depends = attribute_dependencies expr in
         AttrRefSet.fold (fun dependency acc -> (EvalAttr dependency, dependent) :: acc) depends [] in
      List.append (ListExt.concat_map contractual_edges klass.children)
                  (ListExt.concat_map definition_edges klass.definitions) in
   (vertices, edges)

(** Determines an evaluation order for the attributes and children of a class *)
let plan_evaluation contracts klass =
   
   (** Transforms a graph into an adjacency list representation *)
   let adjacency_list (vertices, edges) =
      let adjlist = Hashtbl.create (List.length vertices) in
      ListExt.for_each vertices (fun vertice -> Hashtbl.add adjlist vertice []);
      ListExt.for_each edges (fun (start, target) ->
         let outlist = Hashtbl.find adjlist start in
         Hashtbl.replace adjlist start (target :: outlist));
      adjlist in
   
   (** Consumes an adjacency list searching for a cycle *)
   let find_cycle adjlist =
      (** Finds a node with outgoing edges in the adjacency list *)
      let choose adjlist =
         let fold_choice key xs acc =
            match acc with
             | None -> (match xs with [] -> None | _ -> Some key)
             | Some _ -> acc in
         Hashtbl.fold fold_choice adjlist None in
      (** Removes and returns the first node in an adjacency list for a given key *)
      let pop adjlist node =
         match Hashtbl.find adjlist node with
          | x :: xs -> Hashtbl.replace adjlist node xs; Some x
          | [] -> None in
      (** Walks the graph looking for a cycle meeting a particular start node *)
      let rec walk adjlist goal current acc =
         match pop adjlist current with
          | None -> None
          | Some next ->
            if next = goal then
               Some (List.rev (next :: acc))
            else
               match walk adjlist goal next (next :: acc) with
                | None -> walk adjlist goal current acc
                | cycle -> cycle in
      (** Initiates a walk of the graph with an arbitrary start node until a cycle is found *)
      let rec search adjlist =
         match choose adjlist with
          | None -> None
          | Some start ->
             match walk adjlist start start [start] with
              | None -> search adjlist
              | cycle -> cycle in
      search adjlist in
   
   (** Consumes an adjacency list to produce a topological sort, or returns a cycle on failure *)
   let topological_sort adjlist =
      (* Iterates over a list stored in an adjacency list, removing list elements from the hashtable as it iterates *)
      let consume adjlist key func =
         let xs = ref (Hashtbl.find adjlist key) in
         while not (ListExt.is_empty !xs) do
            Hashtbl.replace adjlist key (List.tl !xs);
            func (ListExt.pop xs)
         done in
      
      (* Checks if a node is terminal (no incident edges) *)
      let is_terminal node = 
         not (Hashtbl.fold (fun _ targets acc -> acc || List.mem node targets) adjlist false) in
      (* Checks if the graph contains any edges *)
      let is_empty adjlist =
         Hashtbl.fold (fun _ targets acc -> acc && ListExt.is_empty targets) adjlist true in
      (* The initial list of terminal nodes *)
      let terminal_nodes =
         let vertices = Hashtbl.fold (fun key _ acc -> EvalStepSet.add key acc) adjlist EvalStepSet.empty in
         let remove_targets _ targets acc = List.fold_left (flip EvalStepSet.remove) acc targets in
         EvalStepSet.elements (Hashtbl.fold remove_targets adjlist vertices) in
      
      (* Traverse the graph, creating an ordering as we iterate *)
      let ordering = ref [] in
      let terminal = ref terminal_nodes in
      while not (ListExt.is_empty !terminal) do
         let n = ListExt.pop terminal in
         ListExt.push n ordering;
         consume adjlist n (fun m ->
            if is_terminal m then ListExt.push m terminal)
      done;
      
      (* If the entire graph has been consumed and sorted, then there are no cycles in the graph *)
      if is_empty adjlist then
         Right (List.rev !ordering)
      else
         match find_cycle adjlist with
          | None -> failwith "bug: failed to find a topological sort or a cycle"
          | Some cyclic_path -> Left cyclic_path in
   
   (* Checks if an evaluation step is actually in the classes evaluation order. We need to do this
      in order to remove attributes the class depends upon, but does not evaluate itself*)
   let is_part_of_evaluation_plan =
      let definitions =
         let fold_defs acc (attr_ref, _) = AttrRefSet.add attr_ref acc in
         List.fold_left fold_defs AttrRefSet.empty klass.definitions in
      function
       | EvalAttr attr_ref -> AttrRefSet.mem attr_ref definitions
       | _ -> true in
   
   (** A valid evaluation order is any topological sort of the evaluation dependencies *)
   let adjlist = adjacency_list (dependency_graph contracts klass) in
   match topological_sort adjlist with
    | Left cyclic_path ->
         let cycle_string = String.concat " -> " (List.map string_of_eval_step cyclic_path) in
         invalid ("The dependencies of " ^ klass.name ^ " include the cycle " ^ cycle_string)
    | Right ordering -> List.filter is_part_of_evaluation_plan ordering

(** Analyzes a grammar to ensure that it is valid, and returns a map of evaluation orders for the classes in the grammar *)
let analyze ifaces klasses =
   let grammar_contracts = contracts ifaces klasses in
   List.iter (typecheck grammar_contracts) klasses;
   let orderings =
      let fold_ordering acc klass = StringMap.add klass.name (plan_evaluation grammar_contracts klass) acc in
      List.fold_left fold_ordering StringMap.empty klasses in
   orderings

(** Dumps a representation of a class to an output channel *)
let pretty_print out klass =
   Printf.fprintf out "class %s " klass.name;
   if not (ListExt.is_empty klass.interfaces) then
      Printf.fprintf out ": %s " (String.concat ", " klass.interfaces);
   Printf.fprintf out "{\n";
   List.iter (fun field -> Printf.fprintf out "   float %s;\n" field) klass.fields;
   List.iter (fun (child, child_type) -> Printf.fprintf out "   %s %s;\n" child_type child) klass.children;
   output_char out '\n';
   List.iter (fun (attr_ref, expr) -> Printf.fprintf out "   def %s = %s;\n" (string_of_attr attr_ref) (string_of_expr expr)) klass.definitions;
   Printf.fprintf out "};\n"
