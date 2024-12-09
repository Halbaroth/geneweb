type debug_flag = private Internal | Tls

type cfg = private {
  host : string;
  port : int;
  base_dir : string;
  tls : (string * string) option;
  debug_flags : debug_flag list;
}

val parse : unit -> cfg
