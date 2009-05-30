
(** Interface declarations are a named collection of synthesized and inherited attributes *)
type interface
 = Interface of string * attr_decl list   (** Interface (name, attributes) *)
and attr_decl
 = InheritDecl of string
 | SynthesizeDecl of string

(** A class implements several interfaces, has fields (which are mutable and may be dynamic), children (which are
   immutable and have attribute of their own) and defines how attributes are evaluated. A class must define all
   of the synthesized attributes declared by its interfaces as well as all of the inherited attributes for
   its children. *)
type klass
 = {
   name: string;                          (** the class name *)
   interfaces: string list;               (** the interfaces this class conforms to *)
   fields: string list;                   (** the fields in this class (all of type float) *)
   children: (string * string) list;      (** the association list of child nodes [(name, type)] *)
   definitions: (attr_ref * expr) list;   (** the association list of attribute definitions *)
 }
and attr_ref
 = ChildAttrRef of string * string        (** ChildAttrRef (child, attribute) -- a reference to a child's attribute (not necessarily inherited!) *)
 | SelfAttrRef of string                  (** SelfAttrRef (attribute) -- a reference to an attribute on the current node *)
and expr
 = Literal of float
 | FieldRef of string
 | AttrRef of attr_ref
 | BinOp of string * expr * expr

(** An evaluation plan for a class is a statically determined (from just the grammar) set of
    evaluation steps that will result in all of the classes attributes being evaluated. A step
    consists of evaluating an attribute definition or causing a child node to evaluate all
    of its attributes. *)
type eval_step
 = EvalAttr of attr_ref
 | EvalChild of string

(** An instance of a class has a collection of field values (which may be dynamic), a collection of children and a collection of attributes *)
type data
 = Instance of string * (string, field) Hashtbl.t * (string, data) Hashtbl.t * (string, float) Hashtbl.t (** Instance (klass, fields, children, attributes) *)
and field = { mutable dynamic: bool; mutable value: float; }

(** Intermediate code consists of instructions to evaluate an expression and assign its result to an attribute. *)
type icode
 = Inst of data * string * data * expr    (** Inst (attribute owner, attribute, expression context, expression) *) 



let string_of_attr = function
 | ChildAttrRef (child, attr) -> child ^ "@" ^ attr
 | SelfAttrRef attr ->  "@" ^ attr

let rec string_of_expr = function
 | Literal x -> string_of_float x
 | FieldRef field -> field
 | AttrRef attr_ref -> string_of_attr attr_ref
 | BinOp (op, left, right) ->
      "(" ^ string_of_expr left ^ " " ^ op ^ " " ^ string_of_expr right ^ ")"

let string_of_eval_step = function
 | EvalAttr attr -> string_of_attr attr
 | EvalChild child -> child



module AttrRefOrdering =
   struct
      type t = attr_ref
      let compare = compare
   end

module AttrRefSet = Set.Make(AttrRefOrdering)
module AttrRefMap = Map.Make(AttrRefOrdering)

module EvalStepOrdering =
   struct
      type t = eval_step
      let compare = compare
   end

module EvalStepSet = Set.Make(EvalStepOrdering)
