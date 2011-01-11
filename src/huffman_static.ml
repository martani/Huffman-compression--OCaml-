(*Copyright (c) 2011, Martani Fakhrou
All rights reserved.*)

type tree = Leaf of int * char
            | Node of int * tree * tree

(*weight/frequency associated to a node*)
let huffman_node_weight n =
    match n with
        Leaf(i, _) -> i
        | Node (i, _, _) -> i;;

(*node comparer*)
let huffman_node_comparer n1 n2 =
    compare (huffman_node_weight n1) (huffman_node_weight n2);;

(*generic sorting on nodes*)
let sort_huffman_trees_list = 
    List.sort huffman_node_comparer;;

(* each leaf represents a char in a text with its frequency *)
let make_huffman_tree leaves_list =
    (* a pripority queue princip here *)
    let rec maker lst = 
        match lst with
            [] -> Leaf(0, '0') (* we should not arrive here!*)
            | [hd] -> hd
            | hd1::hd2::tl -> let new_node = 
                                    Node((huffman_node_weight hd1 + huffman_node_weight hd2),
                                            hd1, hd2) in
                                maker (sort_huffman_trees_list (new_node::tl))
    
    in
        maker (sort_huffman_trees_list leaves_list);;
        
(* this function makes initial leaves with frequencies for each char 
   func_get_next_char is a function that returns a new char on every call
   Sounds familiar right? the Unix kernel stuff...*)
let initial_huffman_trees_list func_get_next_char =
    let temp = Hashtbl.create 256 in
    (try
        let c = ref '0' in
        while(true) do 
              c := func_get_next_char ();
              let a =
                (try
                    Hashtbl.find temp !c;
                with
                    _ -> 0) in

                Hashtbl.replace temp !c (a + 1) 
    done
    with
        End_of_file -> ());
        
    (* get a list from the hastbl // NASTY CODE*)
    let tmp_chars_list = ref [] in
    Hashtbl.iter (fun c freq -> 
                        tmp_chars_list := ((Leaf(freq, c))::(!tmp_chars_list))
                 ) temp;
    
    !tmp_chars_list;;
    
(* returns an int twhich bytes are represented as an int list*)
let list_to_int lst = 
    let rec aux l i =
        match l with 
            [] -> 0.
            | hd::tl -> (float_of_int hd) *. (2. ** i) +. (aux tl (i +. 1.))
    in 
        int_of_float (aux lst 0.);;

(*returns a hashtable of the form 
   key = char
   value = the path in binary int, how many significant bits in the path
   
   -for max optimization*)
let get_huffman_codes huffman_tree =
    let hash = Hashtbl.create 31 in
    let acc = [] in
    
    let rec aux acc huffman_tree  =
        match huffman_tree with
            | Leaf(w, c) -> Hashtbl.add hash c ((list_to_int acc), List.length acc)
            | Node(w, l, r) -> aux (0::acc) l; aux (1::acc) r
    in
        aux acc huffman_tree;
    hash;;

(* returns the most significat 8 bits of the buffer *)
let make_binary buffer most_sig_bit = 
  let mask8 = lnot ((lnot 0) lsl 8) in
  let bits8 = buffer lsr (most_sig_bit - 8) in
  bits8 land mask8
;;
        
(* returns a char on each call from channel chan *)
let nex_char_reader chan () = input_char chan;;

(* compress file f_in_name *)
let compress f_in_name f_out_name =
    let in_chan = open_in_bin f_in_name in
    let out_chan = open_out_bin f_out_name in
   
    let huffman_trees_list = initial_huffman_trees_list (nex_char_reader in_chan) in
  
    let huffman_tree = make_huffman_tree huffman_trees_list in
  
    let hash_codes = get_huffman_codes huffman_tree in
   
    (* write the tree and the file size (in bytes) in the compressed file *)
    Marshal.to_channel out_chan huffman_tree [];
    Marshal.to_channel out_chan (pos_in in_chan) [];
    
    seek_in in_chan 0;

    let c = ref '0' in
    let i = ref 0 in
    
    let buffer = ref 0 in
    
  (try
        while(true) do 
            c := input_char in_chan;
            
            let binary_path, length = Hashtbl.find hash_codes !c in
          
                    (* add the code of the current character to the buffer
                       everything is represented in binary
                       increment the buffer length i *)
                    buffer := !buffer lsl length;
                    buffer := !buffer lor binary_path;
                    i := !i + length;

          (* while we have a byte to write out*)
          while(!i > 7) do
                                            output_byte out_chan (make_binary !buffer !i);
                                            i := !i - 8;
            
                                            (* deleted the just written byte from the buffer *)
                                            let mask = lnot ((lnot 0) lsl (!i )) in
                                            buffer := !buffer land mask;
                                         done
            done;
    with
        End_of_file -> (*anything left on the buffer?*)
                if !i <> 0 then
                begin
                  (* we don't have a byte? add 0's to the end till we have 8 bits *)
                    while(!i < 8) do
                      buffer := !buffer lsl 1;
                      incr i;
                    done;
                  
                    output_byte out_chan (make_binary !buffer 8);
                end
    );
    
    close_in in_chan;
    close_out out_chan;
    ;;

(* this function takes a buffer, and decodes as much codes as it can
   buffer is a Int64
   nb_uncompressed_chars is a reference on the number of chars left uncompressed
   
   returns number of bits actually decoded *)

let push_out chan tree buffer most_significant_bit nb_uncompressed_chars=
    let rec aux t buffer most_significant_bit deep tmpdeep = 
        match t with
            Leaf(w, c) -> if !nb_uncompressed_chars = 0 then 0
                        else
                        begin
                            decr nb_uncompressed_chars;
                            output_byte chan (int_of_char c);
                          
                            (* here we restart from the begining of the tree for a new code *)
                            aux tree buffer most_significant_bit (deep + tmpdeep) 0
                        end
                          
            |Node (_, l, r) -> 
                    if most_significant_bit = -1 then
                    (* here we run out of bits and we didn't yet reached a leaf *)
                        deep - 1
                    else
                        begin
                          (* go right / left ?*)
                            match ((Int64.to_int (Int64.shift_right buffer most_significant_bit)) land 1) with
                                | 0 -> aux l buffer (most_significant_bit - 1) deep (tmpdeep + 1)
                                | _ -> aux r buffer (most_significant_bit - 1) deep (tmpdeep + 1)
                        end
    in
        aux tree buffer most_significant_bit 0 0;;


(* every read operation is supposed to operate on 8 bits *)
let uncompress f_in_name f_out_name =
    let in_chan = open_in_bin f_in_name in
    let out_chan = open_out_bin f_out_name in
    
    (* read the tree and the original file size*)
    let h_tree = Marshal.from_channel in_chan in
    let file_size = Marshal.from_channel in_chan in
    
    (*c is a 8 bit number*)
    let c = ref 0 in

    (* buffer is a holder of bits read from the compressed file*)
    let buffer = ref Int64.zero in
    let most_significat_bit = ref (-1) in
    let i = ref 0 in
    let rest_bits = ref 0 in
    let nb_uncompressed_chars = ref file_size in
    
    (try
        while(true) do 
            c := input_byte in_chan;
          
            (* add the read byte to the buffer
               update the size of the buffer *)
            buffer := Int64.shift_left !buffer 8;
            buffer := Int64.logor !buffer (Int64.of_int !c);
          
            most_significat_bit := !most_significat_bit + 8;
            incr i;

          (*do we have some 32 bits here?*)
            if !i>0 && !i mod 4 = 0 then
            begin
                rest_bits := push_out out_chan h_tree !buffer !most_significat_bit nb_uncompressed_chars;
                most_significat_bit := !most_significat_bit - !rest_bits - 1;
              
                (* delete the just processed bits *)
                let mask = ref (Int64.lognot (Int64.zero)) in
                mask := Int64.shift_left !mask (!most_significat_bit + 1);
                mask := Int64.lognot !mask;
                buffer := Int64.logand !buffer !mask;
                i := 0;
            end
        done
    with
        End_of_file -> (*anything left on the buffer?*)
                ignore (push_out out_chan h_tree !buffer (!most_significat_bit) nb_uncompressed_chars));

    close_in in_chan;
    close_out out_chan;
    ;;