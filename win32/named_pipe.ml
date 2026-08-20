exception Error of string * int * string
let () = Callback.register_exception "Geneweb_named_pipe.Error" (Error ("", -1, ""))

type t = Unix.file_descr

external create_named_pipe : string -> Unix.file_descr = "geneweb_win32_named_pipe_create_named_pipe"
external connect : Unix.file_descr -> unit = "geneweb_win32_named_pipe_connect"
external disconnect : Unix.file_descr -> unit = "geneweb_win32_named_pipe_disconnect"
external flush : Unix.file_descr -> unit = "geneweb_win32_named_pipe_flush"
external wait : string -> unit = "geneweb_named_pipe_wait"

let create_named_pipe s =
  if String.length s >= 256 then failwith "too long";
  let name = Format.sprintf "\\\\.\\pipe\\%s" s in
  create_named_pipe name
