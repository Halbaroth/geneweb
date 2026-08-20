exception Error of string * int * string

let () =
  Callback.register_exception "Geneweb_named_pipe.Error" (Error ("", -1, ""))

type t = Unix.file_descr

type open_mode =
  | PIPE_ACCESS_DUPLEX
  | PIPE_ACCESS_INBOUND
  | PIPE_ACCESS_OUTBOUND
  | FILE_FLAG_FIRST_PIPE_INSTANCE
  | FILE_FLAG_WRITE_THROUGH
  | FILE_FLAG_OVERLAPPED

type pipe_mode =
  | PIPE_TYPE_BYTE
  | PIPE_TYPE_MESSAGE
  | PIPE_READMODE_BYTE
  | PIPE_READMODE_MESSAGE
  | PIPE_WAIT
  | PIPE_NOWAIT
  | PIPE_ACCEPT_REMOTE_CLIENTS
  | PIPE_REJECT_REMOTE_CLIENTS

type time_out = WAIT_USE_DEFAULT_WAIT | WAIT_WAIT_FOREVER | TIME_OUT of int

external create_named_pipe :
  string -> open_mode list -> pipe_mode list -> int -> int -> int -> t
  = "geneweb_win32_named_pipe_create_named_pipe_bytecode"
    "geneweb_win32_named_pipe_create_named_pipe_native"

external connect_named_pipe : t -> unit
  = "geneweb_win32_named_pipe_connect_named_pipe"

external disconnect_named_pipe : t -> unit
  = "geneweb_win32_named_pipe_disconnect_named_pipe"

external flush_file_buffers : t -> unit
  = "geneweb_win32_named_pipe_flush_file_buffers"

external wait_named_pipe : string -> time_out -> unit
  = "geneweb_win32_named_pipe_wait_named_pipe"

external pipe_unlimited_instances : unit -> int
  = "geneweb_win32_named_pipe_pipe_unlimited_instances"

external to_file_descr : t -> Unix.file_descr = "%identity"

let prefix = "\\\\.\\pipe\\"

let create_named_pipe name modes =
  if not @@ String.starts_with ~prefix name then failwith "wrong format"
  else if String.length name >= 256 + String.length prefix then
    failwith "too long"
  else
    let name = Format.sprintf "\\\\.\\pipe\\%s" name in
    create_named_pipe name modes

let close fd =
  flush_file_buffers fd;
  Unix.close @@ to_file_descr fd
