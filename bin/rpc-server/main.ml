module Json_rpc = Geneweb_rpc.Json_rpc
module Route = Geneweb_rpc.Route

let set_levels dflags =
  Logs.Src.set_level Logs.default (Some Debug);
  let flag_to_src (d : Cmd.dflag) =
    match d with TLS -> Tls.Core.src | RPC -> Geneweb_rpc.Server.src
  in
  List.iter
    (fun flag -> Logs.Src.set_level (flag_to_src flag) (Some Debug))
    dflags

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  set_levels cfg.dflags;
  if Option.is_none cfg.tls then Logs.warn (fun k -> k "Connection unsecure!");
  Geneweb_rpc.Server.start ~interface:cfg.interface ~port:cfg.port
    ~max_connection:3 ~idle:5.
  @@ Route.route [ Route.path "/pingpong" Geneweb_rpc.Route.PingPong.srv ];
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever;
  exit (if Logs.err_count () > 0 then 1 else 0)
