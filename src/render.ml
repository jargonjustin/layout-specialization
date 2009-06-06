
open Ast
open Util
open Graphics

let rotate arr =
  if Array.length arr != 0 then begin
    let last = arr.(Array.length arr - 1) in
    Array.blit arr 0 arr 1 (Array.length arr - 1);
    arr.(0) <- last
  end

(** Executes an action at a particular interval until some condition is met *)
let periodically ?(until=(fun () -> false)) interval action =
  let wait interval t_last t_now =
    let elapsed = t_now -. t_last in
    if elapsed < interval then
      Thread.delay (interval -. elapsed) in
  let rec loop () =
    if until () then () else begin
      let t_start = Unix.gettimeofday () in
      action ();
      let t_stop = Unix.gettimeofday () in
      wait interval t_start t_stop;
      loop ()
    end in
  Thread.join (Thread.create loop ())

(** Trys to find the (width, height) of a tree with a 'width' field and a @height attribute on a child named 'root' *)
let sizeof tree =
  try
    let Instance (_, window_fields, window_children, _) = tree in
    let width = int_of_float (Hashtbl.find window_fields "width").value in
    let Instance (_, _, _, root_attrs) = Hashtbl.find window_children "root" in
    let height = int_of_float (Hashtbl.find root_attrs "height") in
    Some (width, height)
  with Not_found -> None

(** Renders the tree to the screen *)
let render tree =
  (** Color palate to draw backgrounds with (used as a ring) *)
  let colors =
    [|rgb 0x66 0x8c 0xcb; rgb 0x9f 0xd9 0x87; rgb 0x77 0xc4 0x72;
      rgb 0x91 0x6a 0xb3; rgb 0xff 0xfb 0x81; rgb 0xff 0x7c 0x62|] in
  (** Sets the current color to color n in the color palate *)
  let use_color n =
     set_color colors.(n mod Array.length colors) in
  (** Attempts to get the rectangle defined by a node's attributes (in the browsers coordinate space) *)
  let dimensions attributes =
    try
      let x = int_of_float (Hashtbl.find attributes "x")
      and y = int_of_float (Hashtbl.find attributes "y")
      and width = int_of_float (Hashtbl.find attributes "width")
      and height = int_of_float (Hashtbl.find attributes "height") in
      Some (x, y, width, height)
    with Not_found -> None in
  (* Resize the window to match tree so the new values may be used but `paint` *)
  begin match sizeof tree with
  | Some (width, height) -> resize_window width height
  | None -> ()
  end;
  (** Attempts to paint the node's attributes, returning true on success *)
  let paint =
    let sy = size_y () in
    fun depth attributes ->
      match dimensions attributes with
      | Some (x, y, w, h) ->
          use_color depth;
          fill_rect x (sy - y - h) w h;
          true
      | None -> false in
  (** Attempts to paint a node and then recursively draw its children *)
  let rec draw depth node =
    let Instance (_, _, children, attributes) = node in
    let children_depth = if paint depth attributes then depth + 1 else depth in
    Hashtbl.iter (fun _ child ->  draw children_depth child) children in
  clear_graph ();
  draw 0 tree;
  synchronize ()

let time task thunk =
  let tstart = Unix.gettimeofday () in
  let result = thunk () in
  Printf.eprintf "%s took %f milliseconds\n" task ((Unix.gettimeofday () -. tstart) *. 1000.);
  flush stderr;
  result

(** Starts the render loop using the specified tree *)
let run tree =
  open_graph "";
  set_window_title "Specialized Layout Rendering";
  auto_synchronize false;
  periodically ~until:key_pressed 0.01 (fun () -> begin
    time "Render" (fun () -> render tree)
  end);
  close_graph ()
