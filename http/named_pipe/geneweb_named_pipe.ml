exception Error of string * int * string
let () = Callback.register_exception "Geneweb_named_pipe.Error" (Error ("", -1, ""))

external open_pipe : string -> Unix.file_descr = "geneweb_named_pipe_open"
external connect : Unix.file_descr -> unit = "geneweb_named_pipe_connect"
external disconnect : Unix.file_descr -> unit = "geneweb_named_pipe_disconnect"
external wait : string -> unit = "geneweb_named_pipe_wait"

let open_pipe s =
  if String.length s >= 256 then failwith "too long";
  let name = Format.sprintf "\\\\.\\pipe\\%s" s in
  open_pipe name
