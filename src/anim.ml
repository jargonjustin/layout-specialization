
open Ast
open Util

(** Extracts a child by name from a tree *)
let child name = function
 | Instance (_, _, children, _) -> Hashtbl.find children name

(** Extracts a field by name from a tree *)
let field name = function
 | Instance (_, fields, _, _) -> Hashtbl.find fields name

(** Lookups up a child by following a list of children names in a tree *)
let rec lookup node = function
 | [] -> node
 | name :: names -> lookup (child name node) names

(** Creates a linear animation, interpolating the field over [start, stop] *)
let linear field start stop =
   let delta = stop -. start in
   fun updater time -> updater field (start +. time *. delta)

(** Performs a step for each animation and returns the list of animations still needing to be run *)
let rec step updater elapsed animations =
  match animations with
  | [] -> []
  | ((animation, duration) as head) :: tail ->
      let time = min 1. (elapsed /. duration) in
      animation updater time;
      let steppedtail = step updater elapsed tail in
      if elapsed < duration then
        head :: steppedtail
      else
        steppedtail
