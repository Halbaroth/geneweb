type protocol_info

type error =
  | WSANOTINITIALISED
  | WSAENETDOWN
  | WSAEINVAL
  | WSAEAINPROGRESS
  | WSAEMFILE
  | WSAENOBUFS
  | WSAENOTSOCK
  | WSAEFAULT

external init : unit -> unit = "geneweb_win32_init"

external duplicate_socket :
  Unix.file_descr -> int -> (protocol_info, error) result
  = "geneweb_win32_duplicate_socket"

external protocol_info_to_socket : protocol_info -> Unix.file_descr
  = "geneweb_win32_protocol_info_to_socket"

let () = if Sys.win32 then init ()
