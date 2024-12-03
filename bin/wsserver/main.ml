module I = Geneweb_search.Index.Default

let handle_exn exn =
  Logs.err (fun k -> k "fatal error:@ %a" Util.pp_exn exn);
  raise exn

let start indexes (cfg : Cmd.cfg) =
  Lwt.dont_wait
    (fun () ->
      let%lwt (_ : Lwt_io.server) =
        Server.listen (Rpc.dispatch indexes) ~host:cfg.host ~port:cfg.port
          ?tls:cfg.tls ()
      in
      Logs_lwt.info (fun k ->
          k "Server listening on %s:%d..." cfg.host cfg.port))
    handle_exn;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let set_levels debug_flags =
  let flag_to_src = function
    | Cmd.Internal -> Logs.default
    | Tls -> Tls.Core.src
  in
  List.iter
    (fun flag -> Logs.Src.set_level (flag_to_src flag) (Some Debug))
    debug_flags

let ( // ) = Filename.concat

let generate_index_from_file path =
  File.with_in_channel path @@ fun ic ->
  let rec loop t =
    match In_channel.input_line ic with
    | None -> t
    | Some word -> loop (I.add word (Util.SS.singleton word) t)
  in
  loop I.empty

(* Fold iterator on all the places of the database [base]. *)
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

let is_separator c = c = ' ' || c = ',' || c = '-'

let flush_buf acc buf start curr =
  match buf with [] -> acc | _ -> (List.rev buf, start, curr) :: acc

let tokenize s =
  let len = String.length s in
  let rec loop acc buf start curr =
    if curr = len then flush_buf acc buf start curr
    else
      let c = String.get s curr in
      if is_separator c then
        let acc = flush_buf acc buf start curr in
        loop acc [] (curr + 1) (curr + 1)
      else loop acc (c :: buf) start (curr + 1)
  in
  loop [] [] 0 0

let generate_index_from_base cache_dir basename =
  let base = Gwdb.open_base basename in
  let normalize tk = List.map Char.lowercase_ascii tk in
  let preprocess s =
    tokenize s
    |> List.map (fun (tk, _, _) -> normalize tk |> List.to_seq |> String.of_seq)
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

let generate_indexes cache_dir base_dir dict_dir =
  let name path = Filename.(basename path |> chop_extension) in
  (* let acc = *)
    File.walk_folder
      (fun kind acc ->
        match kind with
        | `Dir path when Util.is_gwdb_file path ->
            (name path, generate_index_from_base cache_dir path) :: acc
        | `File _ | `Dir _ -> acc)
      base_dir []
  (* in
  File.walk_folder
    (fun kind acc ->
      match kind with
      | `File path -> (name path, generate_index_from_file path) :: acc
      | `Dir _ -> acc)
    dict_dir acc *)

let init_directories () =
  let base = Xdg.create ~env:Sys.getenv_opt () in
  let cache_dir = Xdg.cache_dir base // "geneweb" in
  let data_dir = Xdg.data_dir base // "geneweb" in
  let dict_dir = data_dir // "dict" in
  File.create_dir cache_dir;
  File.create_dir data_dir;
  File.create_dir dict_dir;
  (cache_dir, data_dir, dict_dir)

let main (cfg : Cmd.cfg) =
  set_levels cfg.debug_flags;
  let cache_dir, data_dir, dict_dir = init_directories () in
  Logs.info (fun k -> k "Generate indexes...");
  let indexes = generate_indexes cache_dir cfg.base_dir dict_dir in
  Logs.info (fun k -> k "%d indexes generated." (List.length indexes));
  if Option.is_none cfg.tls then Logs.warn (fun k -> k "Connection unsecure!");
  start indexes cfg

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  let () = main cfg in
  exit (if Logs.err_count () > 0 then 1 else 0)
