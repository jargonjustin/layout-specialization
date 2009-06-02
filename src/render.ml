
open Ast
open Util
open Graphics

let init ?(title="Specializing Layout Renderer") ?(width=640) ?(height=480) () =
   open_graph (Printf.sprintf " %ix%i" width height);
   set_window_title title;
   auto_synchronize false

let close () =
   close_graph ()

let rec loop () =
   let key = read_key () in
   if key = '\027' then
      ()
   else
      loop ()

(** Trys to resize the window to the trees current dimensions *)
let resize tree = 
   try
      let Instance (_, window_fields, window_children, _) = tree in
      let Instance (_, _, _, root_attrs) = Hashtbl.find window_children "root" in
      let width = int_of_float (Hashtbl.find window_fields "width").value in
      let height = int_of_float (Hashtbl.find root_attrs "height") in
      resize_window width height
   with Not_found -> ()

(** Renders the tree to the screen *)
let render tree =
   (** Color palate to draw backgrounds with (used as a ring) *)
   let colors = [|
      rgb 0x66 0x8c 0xcb;
      rgb 0x9f 0xd9 0x87;
      rgb 0x77 0xc4 0x72;
      rgb 0x91 0x6a 0xb3;
      rgb 0xff 0xfb 0x81;
      rgb 0xff 0x7c 0x62|]
   (** Sets the current color to color n in the color palate *)
   let use_color n =
      set_color colors.(n mod Array.length colors)
   (** Attempts to get the rectangle defined by a node's attributes *)
   let dimensions attributes =
      try
         let x = int_of_float (Hashtbl.find attributes "x")
         and y = int_of_float (Hashtbl.find attributes "y")
         and width = int_of_float (Hashtbl.find attributes "width")
         and height = int_of_float (Hashtbl.find attributes "height") in
         Some (x, size_y () - y - height, width, height)
      with Not_found ->
         None in
   (** Attempts to paint the node's attributes, returning true on success *)
   let paint depth attributes =
      match dimensions attributes with
       | Some (x, y, w, h) ->
            use_color depth;
            fill_rect x y w h;
            true
       | None -> false in
   (** Attempts to paint a node and then recursively draw its children *)
   let rec draw depth node =
      let Instance (_, _, children, attributes) = node in
      let children_depth = if paint depth attributes then depth + 1 else depth in
      Hashtbl.iter (fun _ child ->  draw children_depth child) children in
   draw 0 tree;
   synchronize

(** Starts the render loop using the specified tree *)
let run tree =
   init ();
   resize tree;
   render tree;
   loop ();
   close ()
