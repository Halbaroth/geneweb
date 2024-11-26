type kind = [ `Binary | `Continuation | `Text ]
type message = private { kind : kind; content : Yojson.Safe.t }
type handler = Unix.sockaddr -> message -> message Lwt.t

val of_json : Yojson.Safe.t -> message

val listen :
  handler ->
  host:string ->
  port:int ->
  ?tls:string * string ->
  unit ->
  Lwt_io.server Lwt.t
