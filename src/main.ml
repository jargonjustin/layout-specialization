open Util

let profile = ref false
let dump_tree = ref false
let dump_insts = ref false
let verbose = ref false

let time name thunk =
   if !profile then
      let began = Sys.time () in
      let result = thunk () in
      let elapsed = Sys.time () -. began in
      Printf.eprintf "%s finished in %f milliseconds\n" name (elapsed *. 1000.);
      flush stderr;
      result
   else
      thunk ()

let benchmark grammar_filename data_filename =
   try
      let (ifaces, klasses) = Grammar.parse_file grammar_filename in
      let orderings = Grammar.analyze ifaces klasses in
      let data = Data.parse_file klasses data_filename in
      let instructions = time "Compilation" (fun () -> StreamExt.elements (Data.compile klasses orderings data)) in
      let specialized = time "Specialization" (fun () -> StreamExt.elements (Spec.specialize (Stream.of_list instructions))) in
      time "Execution" (fun () -> Spec.interpret (Stream.of_list instructions));
      if !verbose then
         (Printf.eprintf "Naïve compilation produced %i attribute assignments\n" (List.length instructions);
          Printf.eprintf "Specialization kept %i attribute assignments\n" (List.length specialized);
          flush stderr);
      if !dump_insts then
         (Printf.eprintf "Unspecialized instructions:\n"; Spec.examine stderr (Stream.of_list instructions);
          Printf.eprintf "Specialized instructions:\n"; Spec.examine stderr (Stream.of_list specialized);
          flush stderr);
      if !dump_tree then
         Data.pretty_print stdout data;
   with
    | Grammar.Invalid_grammar err -> prerr_endline ("error: " ^ err); exit 1
    | Data.Parse_error err -> prerr_endline ("error: " ^ err); exit 1

let main () =
   let args = ref [] in
   
   let usage = "Usage: " ^ Sys.argv.(0) ^ " [options] grammar data" in
   let spec = Arg.align [
      ("-p", Arg.Set profile, " Displaying profiling information while executing");
      ("-d", Arg.Set dump_tree, " Display annotated tree after execution");
      ("-v", Arg.Set verbose, " Display statistics during execution");
      ("-i", Arg.Set dump_insts, " Display intermediate code")
   ] in
   Arg.parse spec (fun arg -> args := arg :: !args) usage;
   
   match !args with
    | [data; grammar] -> benchmark grammar data
    | _ -> Arg.usage spec usage

let _ = main ()
