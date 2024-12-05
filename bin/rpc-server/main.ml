module Json_rpc = Geneweb_rpc.Json_rpc

let set_levels dflags =
  Logs.Src.set_level Logs.default (Some Debug);
  let flag_to_src (d : Cmd.dflag) =
    match d with TLS -> Tls.Core.src | RPC -> Geneweb_rpc.Server.src
  in
  List.iter
    (fun flag -> Logs.Src.set_level (flag_to_src flag) (Some Debug))
    dflags

let handle_pong (_ : Unix.sockaddr) request =
  match request with
  | Json_rpc.Request (id, _, _) ->
      Lwt.return @@ Json_rpc.Response.success id (`String "pong")
  | _ -> Lwt.return @@ Json_rpc.Response.invalid_request ()

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  set_levels cfg.dflags;
  if Option.is_none cfg.tls then Logs.warn (fun k -> k "Connection unsecure!");
  Geneweb_rpc.Server.start ~interface:cfg.interface ~port:cfg.port
    ~max_connection:5
  @@ Service.route [ Service.path "pong" handle_pong ];
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever;
  exit (if Logs.err_count () > 0 then 1 else 0)
