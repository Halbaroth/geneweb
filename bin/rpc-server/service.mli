type t

val path : string -> Geneweb_rpc.Protocol.t -> t
val route : t list -> Geneweb_rpc.Server.handler
