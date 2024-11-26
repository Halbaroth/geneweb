type kind = [ `Binary | `Continuation | `Text ]
type message = { kind : kind; content : Bigstringaf.t }
type websocket_handler = Unix.sockaddr -> message -> message Lwt.t

val listen :
  websocket_handler ->
  host:string ->
  port:int ->
  ?tls:string * string ->
  unit ->
  Lwt_io.server Lwt.t
