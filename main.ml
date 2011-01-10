let main () =
   try
     (*Dynamic / Static *)
     let dyn_stat = Sys.argv.(1) in
     
     (*compress / decompress*)
     let comp_uncomp = Sys.argv.(2) in
     
     let file_in = Sys.argv.(3) in
     let file_out = Sys.argv.(4) in
     
     match dyn_stat with
       "dyn" -> (
               match (String.sub comp_uncomp 0 1) with
                 | "c" (* compression *) -> Huffman_dynamic.compress file_in file_out
                 | "d" (* decompression *) -> Huffman_dynamic.uncompress file_in file_out
                 | _ -> raise Exit
                )
       | "stat" -> (
               match (String.sub comp_uncomp 0 1) with
                 | "c" (* compression *) -> Huffman_static.compress file_in file_out
                 | "d" (* decompression *) -> Huffman_static.uncompress file_in file_out
                 | _ -> raise Exit
                   )  
       | _ -> raise Exit
   with
     _ -> Printf.printf "usage : %s <dyn|stat> <c[omp]|d[ecomp]> file_in file_out\n" Sys.argv.(0); exit 2;;

main ();;