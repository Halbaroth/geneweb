let src = Logs.Src.create ~doc:"Pool" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)

type worker = { pid : int } [@@unboxed]

let dummy_worker = { pid = -1 }

type t = { k : int -> unit; workers : worker array }

let close_noerr fd = try Unix.close fd with _ -> ()

type 'a process = in_channel -> out_channel -> 'a

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
  match Unix.fork () with
  | 0 -> (
      try
        while true do
          t.k @@ Unix.getpid ()
        done
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        Log.debug (fun k -> k "%a" pp_exception (e, bt)))
  | pid ->
      Log.debug (fun k -> k "Creating worker %d@?" pid);
      t.workers.(i) <- { pid }

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
  let t = { k; workers = Array.init n (fun _ -> dummy_worker) } in
  for i = 0 to n - 1 do
    add_worker t i
  done;
  while true do
    let p = Unix.waitpid [] (-1) |> fst in
    (* We never run out of children because each time a worker terminates,
       it is immediately replaced with a new one. *)
    assert (p > 0);
    match findi (fun { pid } -> pid = p) t.workers with
    | i, w ->
        Log.debug (fun k -> k "Worker %d is dead, replace it" w.pid);
        add_worker t i
    | exception Not_found -> assert false
  done
