let handle_exn exn =
  Logs.err (fun k -> k "fatal error:@ %a" Util.pp_exn exn);
  raise exn

let start (cfg : Cmd.cfg) =
  let f () =
    let%lwt _server =
      Server.listen Rpc.dispatch ~host:cfg.host ~port:cfg.port ?tls:cfg.tls ()
    in
    Logs_lwt.info (fun k -> k "server listening on %s:%d..." cfg.host cfg.port)
  in
  Lwt.dont_wait f handle_exn;
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

let main (cfg : Cmd.cfg) =
  set_levels cfg.debug_flags;
  if Option.is_none cfg.tls then Logs.warn (fun k -> k "Connection unsecure!");
  match cfg.tls with
  | Some _ ->
      Logs.info (fun k -> k "Server starting through TLS...");
      start cfg
  | None ->
      Logs.info (fun k -> k "Server starting...");
      start cfg

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  let () = main cfg in
  exit (if Logs.err_count () > 0 then 1 else 0)
