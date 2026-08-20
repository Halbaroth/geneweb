module Named_pipe = Geneweb_named_pipe

type t = {
  msg : string;
  priority : int
}

let pipe_name = "foo" 

let client () =
  Format.eprintf "[client] started@.";
  Format.eprintf "[client] waiting pipe...@.";
  Named_pipe.wait pipe_name;
  let fd = Unix.openfile "\\\\.\\pipe\\foo" [ Unix.O_RDWR ] 0 in 
  let oc = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) @@ fun () ->
  Marshal.to_channel oc { msg = "Hello"; priority = 18 } [ No_sharing ];
  Format.eprintf "[client] end@."

let spawn_client () =
  let env = Array.append
    [| "IS_CLIENT=yes" |] (Unix.environment ())
  in
  let _ : int = 
    Unix.create_process_env Sys.executable_name Sys.argv env Unix.stdin Unix.stdout Unix.stderr 
  in
  ()

let server () = 
  Format.eprintf "[server] started@.";
  let pipe = Named_pipe.open_pipe pipe_name in
  Format.eprintf "[server] pipe created@.";
  spawn_client ();
  Format.eprintf "[server] waiting for client...@.";
  Named_pipe.connect pipe;
  let ic = Unix.in_channel_of_descr pipe in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
  let { msg; priority } = Marshal.from_channel ic in
  Format.eprintf "[server] msg = %S, priority = %d@." msg priority

let () =
  match Sys.getenv "IS_CLIENT" with 
  | exception Not_found -> server ()
  | _ -> client ()
