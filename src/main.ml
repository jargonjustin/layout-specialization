
open Util
open Ast

let timings = ref false
let verbose = ref false
let specialize = ref true

(** Logs execution times to stderr if timing is enabled *)
let time name thunk =
   if !timings then
      let began = Sys.time () in
      let result = thunk () in
      let elapsed = Sys.time () -. began in
      Printf.eprintf "%s finished in %f milliseconds\n" name (elapsed *. 1000.);
      flush stderr;
      result
   else
      thunk ()

(** Logs to stderr if verbose logging is enabled *)
let log str =
   if !verbose then
      prerr_endline str

(** Dumps grammar dependency information to a channel as a GraphViz graph *)
let dump_dependencies klasses contracts out =
   Grammar.graph_dependencies out klasses contracts

(** Dumps attribute and dynamic dependency information to a channel as a GraphViz graph *)
let dump_dataflow klasses contracts orderings tree out =
   Data.graph out klasses contracts orderings tree

(** Dumps a readable representation of a list of intermediate code instructions to a channel *)
let dump_instructions bytecode out =
   Spec.examine out (Stream.of_list bytecode)

(** Dumps a data tree with attributes to a channel *)
let dump_annotated tree out =
   Data.pretty_print out tree

let run grammar data depsdump flowdump codedump treedump skiprender =
   try
      (* Parse and analyze the grammar *)
      let (ifaces, klasses) = Grammar.parse_channel grammar in
      let (contracts, orderings) = Grammar.analyze ifaces klasses in
      maybe () (dump_dependencies klasses contracts) depsdump;
      
      (* Parse the data tree *)
      let tree = Data.parse_channel klasses contracts data in
      maybe () (dump_dataflow klasses contracts orderings tree) flowdump;
      
      (* Compile and specialize to intermediate code *)
      let bytecode = ref (time "Compilation" (fun () -> StreamExt.elements (Data.compile klasses orderings tree))) in
      log ("Compilation produced " ^ string_of_int (List.length !bytecode) ^ " attribute assignments");
      (* if !specialize then
         (bytecode := time "Specialization" (fun () -> StreamExt.elements (Spec.specialize (Stream.of_list !bytecode)));
          log ("Specialization kept " ^ string_of_int (List.length !bytecode) ^ " attribute assignments")); *)
      maybe () (dump_instructions !bytecode) codedump;
      
      (* Perform any dynamic execution to finish annotation *)
      time "Execution" (fun () -> Spec.interpret (Stream.of_list !bytecode));
      maybe () (dump_annotated tree) treedump;
      
      if not skiprender then
         let margin = Anim.field "margin" (Anim.lookup tree ["root"; "top"]) in
         Render.run !bytecode tree [(Anim.linear margin 10. 100., 5.)]
   with
    | Grammar.Invalid_grammar err -> prerr_endline ("error: " ^ err); exit 1
    | Data.Parse_error err -> prerr_endline ("error: " ^ err); exit 1

let main () =
   try
      let dependency_channel = ref None in
      let dataflow_channel = ref None in
      let bytecode_channel = ref None in
      let annotated_channel = ref None in
      let dont_render = ref false in
   
      let usage = "Usage: " ^ Sys.argv.(0) ^ " [options] grammar treefile" in
      let args =
         let set_channel oref = Arg.String (fun file -> oref := Some (if file = "-" then stdout else open_out file)) in
         Arg.align [
            ("-t",    Arg.Set timings,                        " Display execution time information");
            ("-v",    Arg.Set verbose,                        " Display verbose statistics");
            ("-n",    Arg.Clear specialize,                   " Do not perform specialization");
            ("-r",    Arg.Set dont_render,                    " Do not attempt to render the layout");
            ("-deps", set_channel dependency_channel, "filename Dump a dependency graph of the grammar");
            ("-flow", set_channel dataflow_channel,   "filename Dump a dataflow graph of the tree");
            ("-code", set_channel bytecode_channel,   "filename Dump the intermediate code");
            ("-tree", set_channel annotated_channel,  "filename Dump the annotated tree")
         ] in
   
      let rest = ref [] in
      Arg.parse args (fun anon -> rest := anon :: !rest) usage;
   
      match !rest with
       | [data_filename; grammar_filename] ->
          run (open_in grammar_filename) (open_in data_filename) !dependency_channel !dataflow_channel !bytecode_channel !annotated_channel !dont_render
       | _ -> Arg.usage args usage
   with
    | Sys_error err -> prerr_endline ("error: " ^ err); exit 1

let _ = main ()
