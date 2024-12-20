module I = Index.Default

type 'a loc = { content : 'a; offset : int; len : int }

let is_separator c = c = ' ' || c = ',' || c = '-'

let flush_buf acc buf offset i =
  match buf with
  | [] | [_] | [_;_] ->
      (* Ignore short tokens. *)
      acc
  | _ ->
      let content = List.rev buf |> List.to_seq |> String.of_seq in
      { content; offset; len = i - offset } :: acc

let tokenize s =
  let len = String.length s in
  let rec loop acc buf offset i =
    if i = len then flush_buf acc buf offset i
    else
      let c = String.get s i in
      if is_separator c then
        let acc = flush_buf acc buf offset i in
        loop acc [] (i + 1) (i + 1)
      else loop acc (c :: buf) offset (i + 1)
  in
  loop [] [] 0 0 |> List.rev

let normalize = String.lowercase_ascii

let preprocess s =
  tokenize s |> List.map (fun t -> { t with content = normalize t.content })

let index_from_gzip =
  let rec fold_line f ic acc =
    match My_gzip.input_line ic with
    | exception End_of_file -> acc
    | s -> fold_line f ic (f s acc)
  in
  fun path ->
    My_gzip.with_open path @@ fun ic ->
    fold_line
      (fun content idx ->
        let words = preprocess content in
        List.fold_left (fun idx w -> I.add w.content content idx) idx words)
      ic I.empty

(* (* Fold iterator on all the places of the database [base]. *)
   let fold_places f base acc =
     let g istr = f (Gwdb.sou base istr) in
     let ipers = Gwdb.ipers base in
     (* let ifams = Gwdb.ifams base in *)
     let acc =
       Gwdb.Collection.fold
         (fun acc iper ->
           let p = Gwdb.poi base iper in
           acc |> g (Gwdb.get_birth_place p)
           (* |> g (Gwdb.get_baptism_place p)
              |> g (Gwdb.get_death_place p)
              |> g (Gwdb.get_burial_place p) *))
         acc ipers
     in
     acc
   (* Gwdb.Collection.fold
      (fun acc ifam ->
        let f = Gwdb.foi base ifam in
        g (Gwdb.get_marriage_place f) acc)
      acc ifams *)

   (* let generate_index_from_file path =
      Compat.In_channel.with_open_text path @@ fun ic ->
      let rec loop t =
        match In_channel.input_line ic with
        | None -> t
        | Some word -> loop (I.add word (Util.SS.singleton word) t)
      in
      loop I.empty *)

   let preprocess s =
     tokenize s
     |> List.map (fun (tk, start, end_) ->
            let s = List.to_seq tk |> String.of_seq |> Util.normalize in
            (s, start, end_))

   let generate_index_from_cache =
     let name path = Filename.(basename path |> chop_extension) in
     let rec fold_line f ic acc =
       match My_gzip.input_line ic with
       | exception End_of_file -> acc
       | s -> fold_line f ic (f s acc)
     in
     fun path ->
       My_gzip.with_open path @@ fun ic ->
       ( name path,
         fold_line
           (fun s idx ->
             let words = preprocess s in
             List.fold_left
               (fun idx (word, start, end_) ->
                 I.add word (s, Index.{ offset = start; len = end_ - start }) idx)
               idx words)
           ic I.empty )

   (* let generate_index_from_base _cache_dir basename =
        let base = Gwdb.open_base basename in
        let preprocess s =
          tokenize s
          |> List.map (fun (tk, _, _) ->
                 List.to_seq tk |> String.of_seq |> Util.normalize)
        in
        Fun.protect ~finally:(fun () -> Gwdb.close_base base) @@ fun () ->
        fold_places
          (fun s idx ->
            let words = preprocess s in
            List.fold_left
              (fun idx w ->
                I.update w
                  (fun set_opt ->
                    match set_opt with
                    | Some set -> Some (Util.SS.add s set)
                    | None -> Some (Util.SS.singleton s))
                  idx)
              idx words)
          base I.empty

      let generate_indexes cache_dir base_dir _dict_dir =
        let name path = Filename.(basename path |> chop_extension) in
        (* let acc = *)
        File.walk_folder
          (fun kind acc ->
            match kind with
            | `Dir path when Util.is_gwdb_file path ->
                (name path, generate_index_from_base cache_dir path) :: acc
            | `File _ | `Dir _ -> acc)
          base_dir [] *)
   (* in
      File.walk_folder
        (fun kind acc ->
          match kind with
          | `File path -> (name path, generate_index_from_file path) :: acc
          | `Dir _ -> acc)
        dict_dir acc *)
*)
