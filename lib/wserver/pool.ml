let src = Logs.Src.create ~doc:"Pool" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)

module Worker = struct
  type t = { pid : int; ic : in_channel }

  let dummy = { pid = -1; ic = stdin }

  (* Try to read serialized value on [ic] if the underlying file
     descriptor is ready.

     @raise End_of_file if the file descriptor is ended. *)
  let rec from_channel ?timeout ic =
    let timeout = match timeout with None -> -1. | Some f -> f in
    let fd = Unix.descr_of_in_channel ic in
    match Unix.select [ fd ] [] [] timeout with
    | [], _, _ -> None
    | [ _ ], _, _ -> Some (Marshal.from_channel ic)
    | _ -> assert false

  let is_alive { pid; ic } =
    match from_channel ~timeout:0. ic with
    | Some _ | (exception End_of_file) -> false
    | None -> true

  let kill { pid; _ } =
    try
      Unix.kill pid 0;
      true
    with
    | Unix.Unix_error (Unix.ESRCH, _, _) | Unix.Unix_error (Unix.EPERM, _, _) ->
      false
end

type t = { k : int -> unit; workers : Worker.t array }

let close_noerr fd = try Unix.close fd with _ -> ()

type 'a process = in_channel -> out_channel -> 'a

(* [fork child parent] forks the process and call [child] in the child
   process and [parent] in the current process.

   The channels allows to send data from the child to the parent. *)
let fork (child : out_channel -> 'a) (parent : int -> in_channel -> 'a) =
  let fd_in, fd_out = Unix.pipe () in
  try
    match Unix.fork () with
    | 0 ->
        Unix.close fd_in;
        let oc = Unix.out_channel_of_descr fd_out in
        child oc
    | pid ->
        Unix.close fd_out;
        let ic = Unix.in_channel_of_descr fd_in in
        parent pid ic
  with e ->
    (* Prevents from leaking file descriptors if an exception is raised
       in [child] or [parent] functions. *)
    let bt = Printexc.get_raw_backtrace () in
    close_noerr fd_in;
    close_noerr fd_out;
    Printexc.raise_with_backtrace e bt

(* [double_fork great_child parent] double forks the process and call
   [great_child] in the great child of the current process.

   The Unix pipe allows to send data from the great child to the parent. *)
let double_fork (great_child : out_channel -> int)
    (parent : int -> in_channel -> unit) =
  fork
    (fun oc ->
      (* Child *)
      let gpid, gic =
        fork
          (fun oc ->
            (* Great child *)
            let rc = great_child oc in
            Out_channel.flush oc;
            exit rc)
          (fun gpid gic ->
            (* Child *)
            (gpid, gic))
      in
      Marshal.to_channel oc gpid [];
      let gfd_in = Unix.descr_of_in_channel gic in
      Marshal.to_channel oc gfd_in [];
      Out_channel.flush oc;
      exit 0)
    (fun pid ic ->
      (* Parent *)
      let gpid : int = Marshal.from_channel ic in
      let fd_in : Unix.file_descr = Marshal.from_channel ic in
      let gic = Unix.in_channel_of_descr fd_in in
      let _ = Unix.waitpid [] pid in
      parent gpid gic)

let pp_exception ppf (e, bt) =
  let pp_header ppf pid = Fmt.pf ppf "Exception raised in %d:" pid in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let lines =
    String.split_on_char '\n' @@ Printexc.raw_backtrace_to_string bt
  in
  Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header (Unix.getpid ()) (Printexc.to_string e)
    Fmt.(list ~sep:cut string)
    lines

let add_worker t i =
  Format.eprintf "plop@.";
  let parent pid ic =
    Log.debug (fun k -> k "Creating worker %d@?" pid);
    t.workers.(i) <- { pid; ic }
  in
  let great_child oc =
    try
      while true do
        t.k @@ Unix.getpid ()
      done;
      assert false
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Log.debug (fun k -> k "%a" pp_exception (e, bt));
      Out_channel.output_byte oc 0;
      1
  in
  double_fork great_child parent

let cleanup t =
  for i = 0 to Array.length t.workers - 1 do
    let (_ : bool) = Worker.kill t.workers.(i) in
    ()
  done

let findi (type a) (f : a -> bool) (arr : a array) =
  let exception Found of int * a in
  try
    for i = 0 to Array.length arr - 1 do
      let a = arr.(i) in
      if f a then raise_notrace (Found (i, a))
    done;
    raise Not_found
  with Found (a, i) -> (a, i)

let start n k =
  if n < 1 then invalid_arg "start";
  let t = { k; workers = Array.init n (fun _ -> Worker.dummy) } in
  for i = 0 to n - 1 do
    add_worker t i
  done;
  at_exit (fun () -> cleanup t);
  while true do
    let p = Unix.waitpid [] (-1) |> fst in
    (* We never run out of children because each time a worker terminates,
       it is immediately replaced with a new one. *)
    assert (p > 0);
    match findi (fun Worker.{ pid; _ } -> pid = p) t.workers with
    | i, w ->
        Log.debug (fun k -> k "Worker %d is dead, replace it" w.pid);
        add_worker t i
    | exception Not_found -> assert false
  done
