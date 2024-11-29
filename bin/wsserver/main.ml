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

module I = Geneweb_search.Index.Default

let ( // ) = Filename.concat

let generate_index_from_file path =
  File.with_in_channel path @@ fun ic ->
  let rec loop t =
    match In_channel.input_line ic with
    | None -> t
    | Some line -> loop (I.insert line () t)
  in
  loop I.empty

(* Fold iterator on all the places of the database [base]. *)
let fold_places f base acc =
  let g istr = f (Gwdb.sou base istr) in
  let ipers = Gwdb.ipers base in
  let ifams = Gwdb.ifams base in
  let acc =
    Gwdb.Collection.fold
      (fun acc iper ->
        let p = Gwdb.poi base iper in
        acc
        |> g (Gwdb.get_birth_place p)
        |> g (Gwdb.get_baptism_place p)
        |> g (Gwdb.get_death_place p)
        |> g (Gwdb.get_burial_place p))
      acc ipers
  in
  Gwdb.Collection.fold
    (fun acc ifam ->
      let f = Gwdb.foi base ifam in
      g (Gwdb.get_marriage_place f) acc)
    acc ifams

let generate_index_from_base cache_dir basename =
  let base = Gwdb.open_base basename in
  Fun.protect ~finally:(fun () -> Gwdb.close_base base) @@ fun () ->
  fold_places (fun s -> I.insert s ()) base I.empty

let generate_indexes cache_dir base_dir dict_dir =
  let name path = Filename.(basename path |> chop_extension) in
  let acc =
    File.walk_folder
      (fun kind acc ->
        match kind with
        (* TODO: reactivate this feature after adding the cache system. *)
        | `Dir path when Util.is_gwdb_file path && false ->
            (name path, generate_index_from_base cache_dir path) :: acc
        | `File _ | `Dir _ -> acc)
      base_dir []
  in
  File.walk_folder
    (fun kind acc ->
      match kind with
      | `File path ->
        (name path, generate_index_from_file path) :: acc
      | `Dir _ -> acc)
    dict_dir acc

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
