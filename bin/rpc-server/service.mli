type t

val path : string -> Geneweb_rpc.Server.handler -> t
val route : t list -> Geneweb_rpc.Server.handler
