type 'a limit = Unlimited | Limit of 'a

type t = {
  handles : (Lwt_unix.file_descr, float) Hashtbl.t;
  max_connection : int limit;
  idle : float limit;
}

let limit_of_option = function Some v -> Limit v | None -> Unlimited

let make ?max_connection ?idle () =
  let max_connection = limit_of_option max_connection in
  let idle = limit_of_option idle in
  { handles = Hashtbl.create 17; max_connection; idle }

let add { handles; max_connection; _ } fd =
  match max_connection with
  | Limit i when Hashtbl.length handles >= i -> false
  | _ ->
      let now = Unix.gettimeofday () in
      Hashtbl.replace handles fd now;
      true

let safe_close fd =
  match Lwt_unix.state fd with
  | Opened -> Lwt_unix.close fd
  | _ -> Lwt.return ()

let close { handles; _ } fd =
  Hashtbl.remove handles fd;
  safe_close fd

let close_all { handles; _ } =
  let l =
    Hashtbl.to_seq handles
    |> Seq.map (fun (fd, _) -> safe_close fd)
    |> List.of_seq
  in
  Hashtbl.clear handles;
  Lwt.join l

let ping { handles; idle; _ } fd =
  match idle with
  | Unlimited -> ()
  | Limit _ ->
      let now = Unix.gettimeofday () in
      Hashtbl.replace handles fd now

let gc ({ handles; idle; _ } as t) =
  let now = Unix.gettimeofday () in
  let dead = ref [] in
  Hashtbl.iter
    (fun fd tm ->
      match (Lwt_unix.state fd, idle) with
      | Lwt_unix.(Aborted _ | Closed), _ -> dead := fd :: !dead
      | Opened, Limit v when now -. tm > v -> dead := fd :: !dead
      | _ -> ())
    handles;
  let l = List.map (close t) !dead in
  Lwt.join l

let log_exn exn =
  Logs.err (fun k ->
      k "Uncaught exception in the connection manager:@ %a" Util.pp_exn exn)

let run t =
  let promise, resolver = Lwt.task () in
  let rec loop () =
    match Lwt.state promise with
    | Lwt.Return _ | Lwt.Fail _ -> Lwt.return ()
    | Lwt.Sleep ->
        let%lwt () = Lwt_unix.sleep 2. in
        let%lwt () = gc t in
        loop ()
  in
  Lwt.dont_wait (fun () ->
      let%lwt () = loop () in
      close_all t) log_exn;
  resolver
