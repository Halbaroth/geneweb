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

val duplicate_socket : Unix.file_descr -> int -> (protocol_info, error) result
val protocol_info_to_socket : protocol_info -> Unix.file_descr
