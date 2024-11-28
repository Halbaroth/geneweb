let handle_exn exn =
  Logs.err (fun k -> k "fatal error:@ %a" Util.pp_exn exn);
  raise exn

let start (cfg : Cmd.cfg) =
  Lwt.dont_wait
    (fun () ->
      let%lwt (_ : Lwt_io.server) =
        Server.listen Rpc.dispatch ~host:cfg.host ~port:cfg.port ?tls:cfg.tls ()
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

module Index = Geneweb_search.Index.Default

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

let generate_indexes dir =
  let generate_index fl =
    let base = Gwdb.open_base fl in
    Fun.protect ~finally:(fun () -> Gwdb.close_base base) @@ fun () ->
    fold_places (fun s -> Index.insert s ()) base Index.empty
  in
  Logs.info (fun k -> k "Generate indexes...");
  let indexes, cnt =
    Geneweb.Util.walk_folder
    (fun fl (acc, cnt) ->
      match fl with
      | `Dir fl when Geneweb.Util.is_gwb_dir fl ->
        ((fl, generate_index fl) :: acc, cnt + 1)
      | `File _ | `Dir _ -> (acc, cnt))
    dir ([], 0)
  in
  Logs.info (fun k -> k "%d indexes generated." cnt);
  indexes

let main (cfg : Cmd.cfg) =
  set_levels cfg.debug_flags;
  let indexes = generate_indexes cfg.base_dir in

  let index = List.hd indexes |> snd in
  Fmt.pr "%a@." (Index.pp (Fmt.any "()")) index;

  if Option.is_none cfg.tls then Logs.warn (fun k -> k "Connection unsecure!");
  start cfg

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  let () = main cfg in
  exit (if Logs.err_count () > 0 then 1 else 0)
