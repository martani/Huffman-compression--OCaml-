(* The leaf NYT # *)
let anchor_leaf = DynamicTree.create_anchor_leaf ();;

(* main tree shared among every body *)
let huffman_tree = ref anchor_leaf;;

(* should be sorted with the same apparition of nodes in the tree 
   this array stores references on each node in the order left to right / down to top
   See the adaptive huffman specification for that*)
let blocs_array = Array.make 513 anchor_leaf;;
let next_order = ref 512;;

(* code initial ASCII return list of bits 
   ou codes are all on 8 bits, this way we can compress
   whatever file we want, reardless its coding or nature *)
let char_to_bit_list c = 
    let ri = ref c in
    let l = ref [] in
    for i = 1 to 8 do
        l := (!ri land 1) :: !l;
        ri := !ri lsr 1;
    done;
    !l;;

(* a list of bits => int of 8 bits *)
let bit_list_to_char lst =
    let rec aux l i =
        match l with
            [] -> 0.
        | hd:: tl -> (float_of_int hd) *. (2. ** i) +. (aux tl (i +. 1.))
    in
    int_of_float (aux lst 0.);;

(* table that idicates if we already encountred the char c or not
   the second part of the tuple is useless actually! bad design! *)
let hash_codes: (char, int list) Hashtbl.t = Hashtbl.create 31;;

(* do we see this char for the first time? *)
let belongs_to_tree c =
    Hashtbl.mem hash_codes c;;

(*the leaves we have till now*)
let leaves_list: (char, DynamicTree.tree) Hashtbl.t = Hashtbl.create 31;;

(* the path to the root node [root; n1; n2; ... ;Nn] elements are ref on   
    option tree *)
let rec get_path_to_root n =
    match n with
      None -> []
    | Some(p) -> p::get_path_to_root (DynamicTree.get_node_parent p);;

(* for node n1 and n2, returns 0 if n2 is left son, 1 if n2 is right son 
   of n1 *)
let zero_left_one_right n_parent n_son =
    match n_parent with
        DynamicTree.Leaf(_) -> failwith "a leaf cannot have a son"
    | DynamicTree.Node(_) as n -> if n_son == !(DynamicTree.get_left_child n) then
                0
            else
                begin
                    if n_son == !(DynamicTree.get_right_child n) then
                        1
                    else
                        failwith "n1 is not a son of n2"
                end;;

(* returns huffman code given a path of nodes in the tree *)
let path_to_bit_list path =
    let rec aux p =
        match p with
            [] -> []
        | [n] -> []
        | hd1:: hd2:: tl -> (zero_left_one_right hd1 hd2):: aux (hd2:: tl)
    in
    (aux (List.rev path));;

let get_path_bits n =
    let path = get_path_to_root (Some(n)) in
    path_to_bit_list path;;

exception Stop of DynamicTree.tree;;

(* returns the leaf that holds the char chr *)
let get_leaf_from_chr chr =
    try
        Hashtbl.find leaves_list chr
    with
        _ -> anchor_leaf;;

(* this function deletes i elements from the beggining of the list *)
let rec shift_list lst i =
      if i<0 then lst
        else
    match lst with
        [] -> []
    | _:: tl -> if i = 0 then tl else shift_list tl (i - 1);;

let decode_char lst =
       bit_list_to_char lst;;

let rec first_n_bits lst n =
    match n with
    | 0 -> []
    | x -> (List.hd lst)::(first_n_bits (List.tl lst) (n - 1))
;;



(******* FUNCTIONS THAT TREAT THE TREE UPON CHANGES OF FREQUENCIES **********)

(* this function searches for the highest order node in this 
   class (that have the same weight)*)
let rec fin_bloq node =
    let node_order = DynamicTree.get_node_order node in
    if node_order >= 512 then
        node
    else
        begin
            let next_bloc = blocs_array.(node_order + 1) in
            if(DynamicTree.get_node_weight next_bloc == DynamicTree.get_node_weight node) then
                fin_bloq next_bloc
            else
                node
        end ;;

(* changes the position of 2 nodes in the tree*)
let exchange_nodes n1 n2 =
    let p1 =  match DynamicTree.get_node_parent n1 with None -> assert(false) | Some(p) -> p in
    let p2 = match DynamicTree.get_node_parent n2 with None -> assert(false) | Some(p) -> p in
    let on1 = DynamicTree.get_node_order n1 in
    let on2 = DynamicTree.get_node_order n2 in
    
    (* exchange orders *)
    DynamicTree.set_node_order n1 on2;
    DynamicTree.set_node_order n2 on1;
  
    
    blocs_array.(on1) <- n2;
    blocs_array.(on2) <- n1;
    
    (* exchange trees here if they have the same parent *)
    if p1 == p2 then
        begin
            if DynamicTree.is_left_child p1 n1 then
                begin
                    DynamicTree.link_node_left_child p1 n2;
                    DynamicTree.link_node_right_child p1 n1;
                end
            else
                begin
                    DynamicTree.link_node_left_child p1 n1;
                    DynamicTree.link_node_right_child p1 n2;
                end
        end
    else
      (* not the same parent *)
        begin
            if DynamicTree.is_left_child p1 n1 then
                DynamicTree.link_node_left_child p1 n2
            else
                DynamicTree.link_node_right_child p1 n2;
            
            if DynamicTree.is_left_child p2 n2 then
                DynamicTree.link_node_left_child p2 n1
            else
                DynamicTree.link_node_right_child p2 n1;
            
            DynamicTree.link_node_to_parent n1 p2;
            DynamicTree.link_node_to_parent n2 p1;
        end
;;

(* increments the frequencies of nodes in the tree when possible
   detects incoherencies in node classes => exchange nodes then continues till root*)
let rec treat node =
    let path = get_path_to_root (Some(node)) in
    let referrer = ref anchor_leaf in
    try
        List.iter (fun n ->
                        let order = DynamicTree.get_node_order n in
                        if order >= 512 then
                            DynamicTree.increment_node_weight n
                        else
                            begin
                              (* incoherence, we cannot increment the order of a left/lower node
                                 so that it becomes greater than a right/upper node
                                 
                                 in this cas we search from left to right, down to top for the last
                                 node with the same order as the current node with fin_bloq
                                 
                                 we exchange these nodes, and we increment after*)
                                if DynamicTree.get_node_weight (blocs_array.(order)) = 
                                            DynamicTree.get_node_weight (blocs_array.(order + 1)) then
                                    begin
                                        let b = fin_bloq n in
                                        DynamicTree.increment_node_weight n;
                                        
                                      (* never exchange a node with its parent! *)
                                        if not (DynamicTree.is_left_child b n) 
                                                  && not(DynamicTree.is_right_child b n) then
                                            exchange_nodes n b;
                                        
                                        (match DynamicTree.get_node_parent n with
                                            | None -> referrer := anchor_leaf
                                            | Some(p) -> referrer := p);
                                        
                                       (* we do not continue in the current path since we change the node 
                                          in some other path, we stop, and start over with the new path of node n *)
                                        raise (Stop(n))
                                    end
                                else
                                    (* things are ok here *)
                                    DynamicTree.increment_node_weight n;
                            end
            ) path
    with
    | Stop(n) -> if !referrer == anchor_leaf then () else treat !referrer
;;

(* function that modifies the huffman tree to insert/update/reorder the nodes inside *)
let modify chr =
    let q = ref anchor_leaf in
    if not (belongs_to_tree chr) then
        begin
            (* insert a new leaf, make its parent the parent of the anchor_leaf *)
            let old_parent = DynamicTree.get_node_parent anchor_leaf in
            let leaf = DynamicTree.make_leaf chr in
            let tmp_node = DynamicTree.create_node 0 anchor_leaf leaf old_parent in
            
           (* specify the order of the just created node in the tree -the lowest-*)
            DynamicTree.set_node_order tmp_node !next_order;
            decr next_order;
            DynamicTree.set_node_order leaf !next_order;
            decr next_order;
            DynamicTree.set_node_order anchor_leaf !next_order;
            
            DynamicTree.link_node_to_parent leaf tmp_node;
            DynamicTree.link_node_to_parent anchor_leaf tmp_node;
            
            
            (* add it to the order array *)
            blocs_array.(DynamicTree.get_node_order tmp_node) <- tmp_node;
            blocs_array.(DynamicTree.get_node_order leaf) <- leaf;
            blocs_array.(DynamicTree.get_node_order anchor_leaf) <- anchor_leaf;
            
            (* tree was empty *)
            if old_parent = None then
                huffman_tree := tmp_node
            else
                begin
                    let tmp = match old_parent with None -> assert(false) | Some(p) -> p in
                    DynamicTree.link_node_left_child tmp tmp_node;
                end;

            q := tmp_node;
          
            (* add it to the presence tables *)
            Hashtbl.replace hash_codes chr [];
            Hashtbl.replace leaves_list chr leaf;
        end
    else
        begin
          (* the leaf is to be updated 
             q usually points on the current leaf that has the char chr
             in a special case, q points on the father of chr*)
          
            q := get_leaf_from_chr chr;
            
            if (((DynamicTree.get_node_parent !q) == (DynamicTree.get_node_parent anchor_leaf))
                && ((DynamicTree.get_node_parent !q) = Some(fin_bloq !q))) then
                begin
                    let tmp = match DynamicTree.get_node_parent !q with None -> assert(false) | Some(p) -> p in
                    q := tmp;
                end
        end;
    
    treat !q;;

(********************** END TREE MANIPULATION FUNCTIONS ***********************)

(* compress file f_in_name *)
let compress f_in_name f_out_name =
    let in_chan = open_in_bin f_in_name in
    let out_chan = open_out_bin f_out_name in
    
    (* writes the size of the original file first*)
    let stats = Unix.stat f_in_name in
    let file_size = stats.Unix.st_size in
    Marshal.to_channel out_chan file_size [];
    
  (* there is no binary optimisation here like in the huffman static
     version, so this version is obviously slower than the static one*)
  
    let c = ref '0' in
    let buffer = ref [] in
  
    (try
        while(true) do
            c := input_char in_chan;
                
          (* if the char belongs to the tre, we outpud directly its code
             else, we output the code of the NYT symbole then the 8 bits
             representation of the char*)
          
            if belongs_to_tree !c then
                begin
                    buffer := !buffer @ (get_path_bits (get_leaf_from_chr !c))
                end
            else
                begin
                    buffer := !buffer @ (get_path_bits anchor_leaf);
                    buffer := !buffer @ (char_to_bit_list (int_of_char !c));
                end;
                                               
          (* as long as we have a buffer to output *)
            while (List.length !buffer >= 8) do
                    let lst = first_n_bits !buffer 8 in
                    let byte = bit_list_to_char (List.rev lst) in
                    output_byte out_chan byte;
                    buffer := shift_list !buffer 7;
            done;
          
          (* add / uppdate the tree with the new read char*)
            modify !c;
        done
    with
        End_of_file -> (*anything left on the buffer?*)
            if List.length !buffer <> 0 then
                begin
                    while (List.length !buffer <> 8) do
                        buffer := (!buffer) @ [0];
                    done;
                    
                  output_byte out_chan (bit_list_to_char (List.rev !buffer));
                end
    );
    
    close_in in_chan;
    close_out out_chan;
;;

(* this function takes a buffer, and decodes as much codes as it can
   it returns the number of actual bits decoded 
   the boolean returned indicates if the last decoded char is the NYT symbol or not
   in that case, the uncompress function knows that the next 8 bits it read are
   the actual char itself.
   
   we cannot call this function without the char in the tree already, this is why
   we notify uncompress to take care of the new encountred chars *)
let push_out out_chan tree buffer nb_uncompressed_chars =
    let rec aux t buffer deep tmpdeep =
        match t with
            DynamicTree.Leaf(_) as l ->
        if !nb_uncompressed_chars = 0 then 0, false else begin 
          (* here we restart from the
           begining of the tree for a new code *)
                if DynamicTree.is_anchor_leaf l then
                    begin
                        (*do we have enough bites for the next char to decode?  *)
                        if List.length buffer >= 8 then
                            begin
                                let lst = first_n_bits buffer 8 in
                                let chr = (decode_char (List.rev lst)) in

                                modify (char_of_int chr);
                                output_byte out_chan chr;
                                decr nb_uncompressed_chars;
                                aux tree (shift_list buffer 7) (deep + tmpdeep + 8) 0
                            end
                        else
                            begin
                              (*true means; the next read char is a new char to be decoded*)
                                deep + tmpdeep - 1, true
                            end
                    end
                else
                    begin
                           let chr = (DynamicTree.get_node_value l) in
                            output_byte out_chan (int_of_char chr);
                            modify chr;
                            decr nb_uncompressed_chars;
                            aux tree buffer (deep + tmpdeep) 0
                    end
                                        end
        | DynamicTree.Node (_) as n ->
                if List.length buffer = 0 then
                    (* here we run out of bits and we didn't yet reached a leaf *)
                    deep - 1, false
                else
                    begin
                        match List.hd buffer with
                        | 0 -> aux !(DynamicTree.get_left_child n) (List.tl buffer) deep (tmpdeep + 1)
                        | _ -> aux !(DynamicTree.get_right_child n) (List.tl buffer) deep (tmpdeep + 1)
                    end
    in
    aux tree buffer 0 0;;

(*every read operation is supposed to operate on 8 bits*)
let uncompress f_in_name f_out_name =
    let in_chan = open_in_bin f_in_name in
    let out_chan = open_out_bin f_out_name in
    
    let original_file_size = Marshal.from_channel in_chan in
    
    (*c is a 8 bit number*)
    let c = ref 0 in

    (* buffer represents a list of 1's and 0's // OPTIMISE HERE*)
    let buffer = ref [] in
    let i = ref 0 in
    let rest_bits = ref 0 in
    let nb_uncompressed_chars = ref original_file_size in
    let new_char = ref true in
    
    (try
        while(true) do 
            c := input_byte in_chan;
            buffer := !buffer @ (char_to_bit_list !c);
            incr i;
        
        (* the last time we encountered a new char to be decoded but we
        hadn't enough bits to decode it *)
        if !new_char then
            begin
                let lst = first_n_bits !buffer 8 in
                let chr = decode_char (List.rev lst) in
                                
                decr nb_uncompressed_chars;
                output_byte out_chan chr;
                modify (char_of_int chr);
                buffer := shift_list !buffer 7;
                                
                new_char := false;
            end;
          
        (* useless test? yeah, it was here for optimization then... *)
        if !i > 0 && !i mod 1 = 0 then
            begin
                (* how many bits we couldn't decode? *)
                let res = push_out out_chan !huffman_tree !buffer nb_uncompressed_chars in
                rest_bits := fst res;
                new_char := snd res;
                
                buffer := shift_list !buffer !rest_bits;
                i := 0;
            end;
        done
    with
        End_of_file -> (*anything left on the buffer?*)
                ignore (push_out out_chan !huffman_tree !buffer nb_uncompressed_chars));

    close_in in_chan;
    close_out out_chan;
    ;;