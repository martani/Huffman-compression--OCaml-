(*Copyright (c) 2011, Martani Fakhrou
All rights reserved.*)

(* a mess? the code was built on this data structure and it became too hard to change / simplify it*)
type rec_leaf = { mutable weight_l : int; mutable data : char option; mutable parent_l : tree option ref; mutable order_l: int }
and rec_node = { mutable weight_n : int; mutable left_child: tree ref; mutable right_child: tree ref; mutable parent_n: tree option ref; mutable order_n : int }
and tree = Leaf of rec_leaf | Node of rec_node;;

(* creates a leaf from a char
   A leaf with None as data is te anchor leaf or the NYT *)
let make_leaf c = Leaf({ weight_l = 1; data = Some(c); parent_l = ref None; order_l = 0 });;

(* creates NYT, should be used only for the anchor leaf *)
let create_anchor_leaf () = Leaf({ weight_l = 0; data = None; parent_l = ref None; order_l = 0 });;

(* creates a node with all aspects *)
(* behold!!! parent is a tree option *)
let create_node weight left right parent = 
       Node({
                weight_n = weight;
                    left_child = ref left;
                    right_child = ref right;
                    parent_n = ref parent;
                    order_n = 0
            });;

let get_node_order n = 
       match n with
            | Leaf(r) -> r.order_l
            | Node(r) -> r.order_n;;

let set_node_order n o =
          match n with
            | Leaf(r) -> r.order_l <- o
            | Node(r) -> r.order_n <- o;;

(*change the parent of a node*)
let change_node_parent n p =
       match n with
            | Leaf(r) -> r.parent_l <- ref p
            | Node(r) -> r.parent_n <- ref p;;

let get_node_weight n =
    match n with
        Leaf(r) -> r.weight_l
        | Node(r) -> r.weight_n;;

let increment_node_weight n =
       match n with
            | Leaf(r) -> r.weight_l <- r.weight_l + 1
      | Node(r) -> r.weight_n <- r.weight_n + 1;;

(* comparer *)
let huffman_node_comparer n1 n2 =
    compare (get_node_weight n1) (get_node_weight n2);;

(* generic sorting *)
let sort_huffman_trees_list = 
    List.sort huffman_node_comparer;;

(* returns a ref on an option *)
let get_node_parent n =
    match n with
        Leaf(r) -> !(r.parent_l)
        | Node(r) -> !(r.parent_n);;

let is_left_child p n =
       match p with
            | Leaf(_) -> false
            | Node(r) -> !(r.left_child) == n;;

let is_right_child p n =
       match p with
            | Leaf(_) -> false
            | Node(r) -> !(r.right_child) == n;;

(* only leaves have values = chars = codes *)
let get_node_value n =
    match n with
        Leaf(r) -> (match r.data with None -> char_of_int 0 | Some(x) -> x)
        | Node(_) -> failwith "node cannot have a value";;

(* is n the NYT leaf?*)
let is_anchor_leaf n =
  match n with
    |Leaf(r) -> r.data = None
    | _ -> false;;

(* change the parent of node n*)
let link_node_to_parent n p =
    match n with
        Leaf(r) -> r.parent_l <- ref (Some(p))
        | Node(r) -> r.parent_n <- ref (Some(p));;

(* change the left child of the node n*)
let link_node_left_child n l =
    match n with
        Leaf(_) -> failwith "dude! am a leaf, I have no children!!"
        | Node(r) -> r.left_child <- ref l;;

let link_node_right_child n r =
    match n with
        Leaf(_) -> failwith "dude! am a leaf, I have no children!!"
        | Node(x) -> x.right_child <- ref r;;

let get_left_child n =
    match n with
        Leaf(_) -> failwith "am a leaf, I have no left child"
        | Node (r) -> r.left_child;;

let get_right_child n =
    match n with
        Leaf(_) -> failwith "am a leaf, I have no right child"
        | Node (r) -> r.right_child;;