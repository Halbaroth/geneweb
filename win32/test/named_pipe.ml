type t = { msg : string; priority : int }

let pipe_name = "\\\\.\\pipe\\foo"

let client () =
  Format.eprintf "[client] started@.";
  Format.eprintf "[client] waiting pipe...@.";
  Named_pipe.wait_named_pipe pipe_name WAIT_USE_DEFAULT_WAIT;
  let fd = Unix.openfile pipe_name [ Unix.O_RDWR ] 0 in
  let oc = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) @@ fun () ->
  Marshal.to_channel oc { msg = "Hello"; priority = 18 } [ No_sharing ];
  Format.eprintf "[client] end@."

let spawn_client () =
  let env = Array.append [| "IS_CLIENT=yes" |] (Unix.environment ()) in
  ignore
    (Unix.create_process_env Sys.executable_name Sys.argv env Unix.stdin
       Unix.stdout Unix.stderr
      : int)

let server () =
  Format.eprintf "[server] started@.";
  let fd = Named_pipe.create_named_pipe pipe_name [] [] 4_096 4_096 0 in
  Format.eprintf "[server] pipe created@.";
  spawn_client ();
  Format.eprintf "[server] waiting for client...@.";
  Named_pipe.connect_named_pipe fd;
  let ic = Unix.in_channel_of_descr @@ Named_pipe.to_file_descr fd in
  let finally () =
    (* TODO: we must flush ic? *)
    Named_pipe.close fd
  in
  Fun.protect ~finally @@ fun () ->
  let { msg; priority } = Marshal.from_channel ic in
  Format.eprintf "[server] msg = %S, priority = %d@." msg priority

let () =
  match Sys.getenv "IS_CLIENT" with
  | exception Not_found -> server ()
  | _ -> client ()
