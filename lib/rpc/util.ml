let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX s -> Fmt.string ppf s
  | ADDR_INET (addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr addr) port

let pp_exn ppf exn =
  let s = Printexc.to_string exn in
  if Printexc.backtrace_status () then
    Fmt.pf ppf "%s@ backtrace:@ %s" s (Printexc.get_backtrace ())
  else Fmt.string ppf s

open Lwt.Infix

(* Similar implementation of Fun.protect but compatible with Lwt. *)
let protect ~finally f =
  try%lwt f () >>= finally with exn -> finally () >>= Lwt.reraise exn
