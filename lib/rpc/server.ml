module Body = Httpun.Body
module Headers = Httpun.Headers
module Reqd = Httpun.Reqd
module Response = Httpun.Response
module Status = Httpun.Status
module Server = Httpun_lwt_unix.Server
module Y = Yojson.Safe
module U = Yojson.Safe.Util
open Lwt.Infix

(* This module is responsible for configuring the server of `httpun-ws` for our
   specific use case. The only challenging part lies in properly handling and
   logging all errors. We need to manage and log the following:

     1. DNS and socket errors encountered while establishing the server.
     2. TLS and socket errors that occur when a client attempts to connect.
     3. Encoding/Decoding errors in the RPC handler.
     4. Uncaught exceptions in the user handler. *)

let src = Logs.Src.create "RPC"

module Log = (val Logs.src_log src : Logs.LOG)

type handler =
  Unix.sockaddr -> string -> Json_rpc.request -> Json_rpc.response Lwt.t

let response_to_string response =
  Y.to_string @@ Json_rpc.Response.to_json response

let rpc_handler handler sockaddr target content =
  try
    let j = Y.from_string content in
    match Json_rpc.Request.of_json j with
    | Some request ->
        Logs_lwt.debug ~src (fun k ->
            k "Received the Request object from %a:@ %a" Util.pp_sockaddr
              sockaddr Json_rpc.Request.pp request)
        >>= fun () ->
        handler sockaddr target request >>= fun response ->
        Logs_lwt.debug ~src (fun k ->
            k "Response message to %a:@ %a" Util.pp_sockaddr sockaddr
              Json_rpc.Response.pp response)
        >>= fun () -> Lwt.return @@ response_to_string response
    | None ->
        Logs_lwt.debug ~src (fun k ->
            k "The client sent an invalid Request object:@ %a"
              (Y.pretty_print ~std:true) j)
        >>= fun () ->
        Lwt.return @@ response_to_string @@ Json_rpc.Response.invalid_request ()
  with U.Type_error (s, _) ->
    let%lwt () =
      Logs_lwt.debug ~src (fun k ->
          k "The client sent an invalid JSON message:@ %s@ Parser error:@ %s"
            content s)
    in
    Lwt.return @@ response_to_string @@ Json_rpc.Response.parse_error ()

(* These exceptions should never occur in production. If such an exception
   is raised, it indicates that the user handler has not handled it correctly.
   This will not terminate the server, but the WebSocket connection will be
   lost. *)
let log_ws_handler_exn sockaddr exn =
  Log.debug (fun k ->
      k "Exception raised while processing request for %a:@ %a" Util.pp_sockaddr
        sockaddr Util.pp_exn exn)

let ws_handler manager fd rpc_handler sockaddr target wsd =
  let on_read ~kind content ~off:_ ~len:_ =
    let () = Connection.ping manager fd in
    Lwt.dont_wait
      (fun () ->
        rpc_handler sockaddr target @@ Bigstringaf.to_string content
        >>= fun content ->
        let len = String.length content in
        let content = Bigstringaf.of_string ~off:0 ~len content in
        Lwt.return @@ Httpun_ws.Wsd.schedule wsd ~kind content ~off:0 ~len)
      (log_ws_handler_exn sockaddr)
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
    | Some _ -> assert false
    | None ->
        Log.err (fun k -> k "EOF");
        Httpun_ws.Wsd.close wsd
  in
  { Httpun_ws.Websocket_connection.frame; eof }

module Request = Httpun.Request

(* The HTTP request handler only accepts upgrade request to the WebSocket
   protocol. *)
let http_request_handler ws_handler sockaddr
    (reqd : Httpun.Reqd.t Gluten.Reqd.t) =
  let Httpun.Request.{ target; _ } = Reqd.request reqd.reqd in
  Log.info (fun k ->
      k "Ask for upgrading connection by %a on target %s" Util.pp_sockaddr
        sockaddr target);
  let handler () =
    let ws_conn =
      Httpun_ws.Server_connection.create_websocket (ws_handler sockaddr target)
    in
    reqd.upgrade (Gluten.make (module Httpun_ws.Server_connection) ws_conn)
  in
  match
    Httpun_ws.Handshake.respond_with_upgrade ~sha1:Util.sha1 reqd.reqd handler
  with
  | Ok () ->
      Log.info (fun k -> k "Successful upgrade by %a" Util.pp_sockaddr sockaddr)
  | Error e ->
      (* We immediately close the connection if the client does not pass
         scrunity. *)
      Log.info (fun k ->
          k "Handshake failed by %a:@ %s" Util.pp_sockaddr sockaddr e);
      let headers = Httpun.Headers.of_list [ ("Connection", "close") ] in
      let response = Response.create ~headers `Bad_request in
      Reqd.respond_with_string reqd.reqd response e

(* In case of HTTP error, we simply return the error to the client. *)
let http_error_handler _sockaddr ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error ->
        Status.to_string error
  in
  let body = handle Headers.empty in
  Body.Writer.write_string body message;
  Body.Writer.close body

(* If an exception reaches this handler, it should indicate an error while
   establishing the server socket. This is considered an unrecoverable error,
   and the program must terminate. Any other exceptions should be treated as
   bugs, as they should have been handled earlier. *)
let log_server_exn exn =
  Log.err (fun k -> k "Uncaught exception in the server:@ %a" Util.pp_exn exn);
  raise exn

let start ~interface ~port ?max_connection ?idle ?(tls = false) ?certfile
    ?keyfile user_handler =
  let manager = Connection.make ?max_connection ?idle () in
  let stop_gc = Connection.run manager in
  let create_connection_handler =
    match (tls, certfile, keyfile) with
    | true, Some certfile, Some keyfile ->
        fun request_handler ->
          Server.TLS.create_connection_handler_with_default ~certfile ~keyfile
            ?config:None ~request_handler ~error_handler:http_error_handler
    | false, _, _ ->
        fun request_handler ->
          Server.create_connection_handler ?config:None ~request_handler
            ~error_handler:http_error_handler
    | _ -> Fmt.invalid_arg "start"
  in
  let connection_handler sockaddr fd =
    if Connection.add manager fd then
      let request_handler =
        http_request_handler @@ ws_handler manager fd @@ rpc_handler
        @@ user_handler
      in
      try%lwt
        Util.protect
          ~finally:(fun () ->
            Connection.close manager fd >>= fun () ->
            Logs_lwt.info ~src (fun k ->
                k "Close connection of %a" Util.pp_sockaddr sockaddr))
          (fun () -> create_connection_handler request_handler sockaddr fd)
      with exn ->
        (* Exceptions can be raised in the client connection handler itself if
           the connection or the TLS negotiation fail. Any other exceptions
           should be treated as bugs, as they should have been handled
           earlier. *)
        Logs_lwt.info ~src (fun k ->
            k "Uncaught exception raised in connection handler of %a:@ %a"
              Util.pp_sockaddr sockaddr Util.pp_exn exn)
    else
      Logs_lwt.info ~src (fun k ->
          k "Refused the connection from %a" Util.pp_sockaddr sockaddr)
  in
  Lwt.dont_wait
    (fun () ->
      Lwt_unix.getaddrinfo interface (Int.to_string port)
        [ Unix.(AI_FAMILY PF_INET) ]
      >>= fun addresses ->
      let sockaddr = (List.hd addresses).Unix.ai_addr in
      Lwt_io.establish_server_with_client_socket ~no_close:true sockaddr
        connection_handler
      >>= fun (_ : Lwt_io.server) ->
      Logs_lwt.info ~src (fun k ->
          k "Server listening on %a..." Util.pp_sockaddr sockaddr))
    log_server_exn
