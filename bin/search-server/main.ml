module I = Geneweb_search.Index.Default
module Index = Geneweb_search.Index

let handle_exn exn =
  Logs.err (fun k -> k "fatal error:@ %a" Util.pp_exn exn);
  raise exn

let start indexes (cfg : Cmd.cfg) =
  Lwt.dont_wait
    (fun () ->
      let%lwt (_ : Lwt_io.server) =
        Server.listen (Rpc.dispatch indexes) ~host:cfg.interface ~port:cfg.port
          ?tls:cfg.tls ()
      in
      Logs_lwt.info (fun k ->
          k "Server listening on %s:%d..." cfg.interface cfg.port))
    handle_exn;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let set_levels dflags =
  let flag_to_src = function
    | Cmd.Server -> Logs.default
    | TLS -> Tls.Core.src
  in
  List.iter
    (fun flag -> Logs.Src.set_level (flag_to_src flag) (Some Debug))
    dflags

let ( // ) = Filename.concat

let main (cfg : Cmd.cfg) =
  set_levels cfg.dflags;
  Logs.info (fun k -> k "Generate indexes...");
  let path =
    "./distribution/bases/etc/roglo-v7/cache/roglo-v7_places.cache.gz"
  in
  let name = Filename.basename path |> Filename.remove_extension in
  let index = Geneweb_search.Analyze.index_from_gzip path in
  (* Logs.info (fun k -> k "%s: %a" name I.pp_statistics index); *)
  (* let indexes = generate_indexes cache_dir cfg.base_dir dict_dir in *)
  (* Logs.info (fun k -> k "%d indexes generated." (List.length indexes)); *)
  if Option.is_none cfg.tls then Logs.warn (fun k -> k "Connection unsecure!");
  start [ (name, index) ] cfg

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  let () = main cfg in
  exit (if Logs.err_count () > 0 then 1 else 0)
