
module OrderedString =
   struct
      type t = string
      let compare = compare
   end

module StringSet = Set.Make(OrderedString)
module StringMap = Map.Make(OrderedString)

(** E *)
module ListExt =
   struct
      let filter_map f xs =
         let cons_option x ys =
            match f x with
             | Some y -> y :: ys
             | None   -> ys
         in List.fold_right cons_option xs []
      let concat_map f xs =
         List.concat (List.map f xs)
      let for_each xs f =
         List.iter f xs
      let is_empty xs =
         match xs with
          | [] -> true
          | _ -> false
      let pop rxs =
         match !rxs with
          | head :: tail -> rxs := tail; head
          | [] -> failwith "empty list"
      let push x rxs =
         rxs := x :: !rxs
   end

(** Identity function *)
let identity x = x

(** Reverses the order of a functions first two arguments *)
let flip f y x = f x y

(** The either type allows for two possibly different types to be returned by a function.
    It is conventionally used for computations that may produce an error, with error values
    being returned in Left and success values being returned in Right. *)
type ('a, 'b) either
 = Left of 'a
 | Right of 'b

module StreamExt =
   struct
      let rec map f = function
       | [] -> [< >]
       | (x :: xs) -> [< f x; map f xs >]
      let rec filter_map f stream =
         try
            match f (Stream.next stream) with
             | Some x -> [< 'x; filter_map f stream >]
             | None -> filter_map f stream
         with Stream.Failure -> [< >]
      let rec elements = parser
       | [< 'x; xs = elements >] -> x :: xs
       | [< >] -> []
   end

module HashtblExt =
   struct
      let search ht p =
         let fold_search k v acc =
            match acc with
             | Some _ -> acc
             | None -> if p k v then Some (k, v) else None in
         match Hashtbl.fold fold_search ht None with
          | Some result -> result
          | None -> raise Not_found
   end
