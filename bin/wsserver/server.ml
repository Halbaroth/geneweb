module Body = Httpun.Body
module Headers = Httpun.Headers
module Reqd = Httpun.Reqd
module Response = Httpun.Response
module Status = Httpun.Status

type kind = [ `Binary | `Continuation | `Text ]
type message = { kind : kind; content : Yojson.Safe.t }
type handler = Unix.sockaddr -> message -> message Lwt.t

let of_json json =
  (* TODO: do validation here *)
  { kind = `Text; content = json }

let error_handler _sockaddr ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error ->
        Status.to_string error
  in
  let body = handle Headers.empty in
  Body.Writer.write_string body message;
  Body.Writer.close body

let connection_handler ?tls =
  match tls with
  | Some (certfile, keyfile) ->
      Httpun_lwt_unix.Server.TLS.create_connection_handler_with_default
        ~certfile ~keyfile ?config:None
  | None -> Httpun_lwt_unix.Server.create_connection_handler ?config:None

let connection_handler ~request_handler ?tls socket fd =
  try%lwt connection_handler ~request_handler ~error_handler ?tls socket fd with
  | (Sys.Break | Assert_failure _ | Match_failure _) as exn ->
      let%lwt () =
        Logs_lwt.err (fun k ->
            k "Fatal error in connection handler of %a:@ %a" Util.pp_sockaddr
              socket Util.pp_exn exn)
      in
      Lwt.reraise exn
  | exn ->
      Logs_lwt.info (fun k ->
          k "Exception raised in connection handler of %a:@ %a" Util.pp_sockaddr
            socket Util.pp_exn exn)

let catch_exn exn =
  match exn with
  | (Sys.Break | Assert_failure _ | Match_failure _) as exn ->
    raise exn
  | exn ->
    Logs.debug (fun k -> k "got %a" Util.pp_exn exn)

let listen handler ~host ~port ?tls () =
  let websocket_handler sockaddr wsd =
    let on_read content ~kind ~off:_ ~len:_ =
      Lwt.dont_wait (fun () ->
      let json = Bigstringaf.to_string content |> Yojson.Safe.from_string in
      let%lwt () =
        Logs_lwt.debug (fun k ->
            k "Receive message from %a:@ %a" Util.pp_sockaddr sockaddr
              (Yojson.Safe.pretty_print ~std:false)
              json)
      in
      let request = { kind; content = json } in
      let%lwt msg = handler sockaddr request in
      try
        let response = Yojson.Safe.to_string msg.content in
        let len = String.length response in
        let response = Bigstringaf.of_string ~off:0 ~len response in
        let%lwt () =
          Logs_lwt.debug (fun k ->
              k "Send message to %a:@ %a" Util.pp_sockaddr sockaddr
                (Yojson.Safe.pretty_print ~std:false) msg.content)
        in
        Lwt.return @@ Httpun_ws.Wsd.schedule wsd response ~kind ~off:0 ~len
      with Yojson.Json_error s ->
        Logs_lwt.debug (fun k -> k "cannot parse the json:@ %s" s))
      catch_exn
    in
    let frame ~opcode ~is_fin:_ ~len:_ payload =
      match (opcode : Httpun_ws.Websocket.Opcode.t) with
      | #Httpun_ws.Websocket.Opcode.standard_non_control as opcode ->
          Httpun_ws.Payload.schedule_read payload ~on_eof:ignore
            ~on_read:(on_read ~kind:opcode)
      | `Connection_close -> Httpun_ws.Wsd.close wsd
      | `Ping -> Httpun_ws.Wsd.send_pong wsd
      | `Pong | `Other _ -> ()
    in
    let eof ?error () =
      match error with
      | Some _ ->
          (* TODO: check if this branch is unreachable. *)
          assert false
      | None ->
          Logs.err (fun k -> k "EOF");
          Httpun_ws.Wsd.close wsd
    in
    { Httpun_ws.Websocket_connection.frame; eof }
  in
  let upgrade_handler sockaddr upgrade () =
    let ws_conn =
      Httpun_ws.Server_connection.create_websocket (websocket_handler sockaddr)
    in
    upgrade (Gluten.make (module Httpun_ws.Server_connection) ws_conn)
  in
  let request_handler sockaddr (reqd : Httpun.Reqd.t Gluten.Reqd.t) =
    let handler = upgrade_handler sockaddr reqd.upgrade in
    match
      Httpun_ws.Handshake.respond_with_upgrade ~sha1:Util.sha1 reqd.reqd handler
    with
    | Ok () -> ()
    | Error err_str ->
        (* We immediately close the connection if the client does not pass
           scrunity. *)
        let headers = Httpun.Headers.of_list [ ("Connection", "close") ] in
        let response = Response.create ~headers `Bad_request in
        Reqd.respond_with_string reqd.reqd response err_str
  in
  let%lwt addresses =
    Lwt_unix.getaddrinfo host (Int.to_string port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  let sockaddr = (List.hd addresses).Unix.ai_addr in
  Lwt_io.establish_server_with_client_socket sockaddr
    (connection_handler ~request_handler ?tls)
