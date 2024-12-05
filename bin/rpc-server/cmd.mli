type dflag = private TLS | RPC

type cfg = private {
  interface : string;
  port : int;
  base_dir : string;
  index_dir : string;
  tls : (string * string) option;
  dflags : dflag list;
}

val parse : unit -> cfg
