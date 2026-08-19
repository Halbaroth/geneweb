module Named_pipe = Geneweb_named_pipe

let pipe_name = "foo" 

let client () =
  Format.eprintf "[client] started@.";
  Format.eprintf "[client] waiting pipe...@.";
  Named_pipe.wait pipe_name;
  let fd = Unix.openfile "\\\\.\\pipe\\foo" [ Unix.O_RDWR ] 0 in 
  let bytes = Bytes.of_string "Hello" in
  let w = Unix.write fd bytes 0 (Bytes.length bytes) in
  assert (w = Bytes.length bytes);
  Unix.close fd;
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
  let buf = Bytes.create 4096 in
  let r = Unix.read pipe buf 0 4096 in
  let content = Bytes.sub_string buf 0 r in
  Format.eprintf "result: %s@." content

let () =
  match Sys.getenv "IS_CLIENT" with 
  | exception Not_found -> server ()
  | _ -> client ()
