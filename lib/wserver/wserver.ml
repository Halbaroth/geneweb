(* Copyright (c) 1998-2007 INRIA *)

let eprintf = Printf.eprintf
let sock_in = ref "wserver.sin"
let sock_out = ref "wserver.sou"

(* global parameters set by command arguments *)
let stop_server = ref "STOP_SERVER"
let cgi = ref false
let no_fork = ref false

(* state of a connection request *)
let connection_closed = ref false
let wserver_sock = ref Unix.stdout
let wserver_oc = ref stdout

(* functions to access the connection state *)
let wsocket () = !wserver_sock
let woc () = !wserver_oc
let wflush () = flush !wserver_oc

let rec is_data_available ~timeout fd =
  let start = Unix.gettimeofday () in
  match Unix.select [ fd ] [] [] timeout with
  | exception Unix.Unix_error (Unix.EINTR, "select", "") ->
      (* The system call can be interrupted by a signal and we need to recall
         it immediately. This happens when a child process terminates and sends
         the signal SIGCHLD. *)
      let stop = Unix.gettimeofday () in
      let timeout = max 0. (timeout -. (stop -. start)) in
      is_data_available ~timeout fd
  | [ _ ], _, _ -> true
  | _ -> false

let rec loop_wait pid =
  match Unix.wait pid with
  | exception Unix.Unix_error (Unix.EINTR, "wait", "") ->
      (* The system call can be interrupted by a signal and we need to recall
         it immediately. This happens when a child process terminates and sends
         the signal SIGCHLD. *)
      loop_wait pid
  | r -> r

let rec loop_accept socket =
  match Unix.accept socket with
  | exception Unix.Unix_error (Unix.EINTR, "accept", "") ->
      (* The system call can be interrupted by a signal and we need to recall
         it immediately. This happens when a child process terminates and sends
         the signal SIGCHLD. *)
      loop_accept socket
  | r -> r

let skip_possible_remaining_chars fd =
  let b = Bytes.create 3 in
  try
    let rec loop () =
      (* We do not need to catch [Unix.EINTR] because this function
         is only called in children if [!no_fork] is [false] or
         the signal SIGCHLD is not caught if [!no_fork] is [true]. *)
      match Unix.select [ fd ] [] [] 5.0 with
      | [ _ ], [], [] ->
          let len = Unix.read fd b 0 (Bytes.length b) in
          if len = Bytes.length b then loop ()
      | _ -> ()
    in
    loop ()
    (* Read on https://utcc.utoronto.ca/~cks/space/blog/unix/AcceptErrnoProblem:
       These days accept() is standardized to return ECONNABORTED instead of
       ECONNRESET in these circumstances, although this may not be universal.
    *)
  with Unix.Unix_error (Unix.(ECONNRESET | ECONNABORTED), _, _) -> ()

let close_connection () =
  if not !connection_closed then (
    wflush ();
    (try Unix.shutdown !wserver_sock Unix.SHUTDOWN_SEND with _ -> ());
    skip_possible_remaining_chars !wserver_sock;
    (* Closing the channel flushes the data and closes the underlying file descriptor *)
    close_out !wserver_oc;
    connection_closed := true)

let printnl () = output_string !wserver_oc "\013\010"

type printing_state = Nothing | Status | Contents

let printing_state = ref Nothing

let http status =
  if !printing_state <> Nothing then failwith "HTTP Status already sent";
  printing_state := Status;
  if status <> Def.OK || not !cgi then (
    let answer =
      match status with
      | Def.OK -> "200 OK"
      | Def.Moved_Temporarily -> "302 Moved Temporarily"
      | Def.Bad_Request -> "400 Bad Request"
      | Def.Unauthorized -> "401 Unauthorized"
      | Def.Forbidden -> "403 Forbidden"
      | Def.Not_Found -> "404 Not Found"
      | Def.Conflict -> "409 Conflict"
      | Def.Internal_Server_Error -> "500 Internal Server Error"
      | Def.Service_Unavailable -> "503 Service Unavailable"
    in
    if !cgi then (
      output_string !wserver_oc "Status: ";
      output_string !wserver_oc answer)
    else (
      output_string !wserver_oc "HTTP/1.0 ";
      output_string !wserver_oc answer);
    printnl ())

let header s =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
  output_string !wserver_oc s;
  printnl ()

let printf fmt =
  if !printing_state <> Contents then (
    if !printing_state = Nothing then http Def.OK;
    printnl ();
    printing_state := Contents);
  Printf.fprintf !wserver_oc fmt

let print_string s =
  if !printing_state <> Contents then (
    if !printing_state = Nothing then http Def.OK;
    printnl ();
    printing_state := Contents);
  output_string !wserver_oc s

let http_redirect_temporarily url =
  http Def.Moved_Temporarily;
  output_string !wserver_oc "Location: ";
  output_string !wserver_oc url;
  printnl ()

let buff = ref (Bytes.create 80)

let store len x =
  if len >= Bytes.length !buff then
    buff := Bytes.extend !buff 0 (Bytes.length !buff);
  Bytes.set !buff len x;
  succ len

let get_buff len = Bytes.sub_string !buff 0 len

let get_request strm =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then []
        else
          let str = get_buff len in
          str :: loop 0 s
    | Some '\013' ->
        Stream.junk strm__;
        loop len strm__
    | Some c ->
        Stream.junk strm__;
        loop (store len c) strm__
    | _ -> if len = 0 then [] else [ get_buff len ]
  in
  loop 0 strm

let get_request_and_content strm =
  let request = get_request strm in
  let content =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  (request, Adef.encoded content)

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a

let sockaddr_of_string s = Unix.ADDR_UNIX s

let timeout_handler ~timeout _ =
  try
    if !printing_state = Nothing then http Def.OK;
    if !printing_state <> Contents then (
      output_string !wserver_oc "Content-type: text/html; charset=iso-8859-1";
      printnl ();
      printnl ();
      printf "<head><title>Time out</title></head>\n";
      printf "<body>");
    printf "<h1>Time out</h1><p>Computation time > %d second(s)</p></body>"
      timeout;
    wflush ();
    exit 0
  with Sys_error _ ->
    (* The client may close the connection before reaching the time limit.
       In this case, we cannot print the timeout message to the socket but
       this is not an error and we must exit normally, even in no-fork mode. *)
    exit 0

(* Set a Unix signal with a timeout around the execution of the function [f].
   The signal is properly cleared even if the function [f] raises an exception.

   Since a process can have only one active alarm signal at a time, this
   function should be used only once per fork of the web server.

   This function is supported only on Unix. *)
let with_timeout ~timeout handler f =
  assert Sys.unix;
  let (_ : Sys.signal_behavior) =
    Sys.signal Sys.sigalrm (Sys.Signal_handle handler)
  in
  let finally () =
    let (_ : int) = Unix.alarm 0 in
    ()
  in
  let g () =
    let (_ : int) = Unix.alarm timeout in
    f ()
  in
  Fun.protect ~finally g

let treat_connection callback addr fd =
  printing_state := Nothing;
  let request, path, query =
    let request, query =
      let strm = Stream.of_channel (Unix.in_channel_of_descr fd) in
      get_request_and_content strm
    in
    let path, query =
      match Mutil.extract_param "GET /" ' ' request with
      | "" -> (Mutil.extract_param "POST /" ' ' request, query)
      | str -> (
          match String.index_opt str '?' with
          | Some i ->
              ( String.sub str 0 i,
                String.sub str (i + 1) (String.length str - i - 1)
                |> Adef.encoded )
          | None -> (str, "" |> Adef.encoded))
    in
    (request, path, query)
  in
  callback (addr, request) path query

let treat_connection_with_timeout ~timeout callback addr fd =
  if Sys.unix && timeout > 0 then
    with_timeout ~timeout (timeout_handler ~timeout) @@ fun () ->
    treat_connection callback addr fd
  else treat_connection callback addr fd

let buff = Bytes.create 1024

let copy_what_necessary t oc =
  let strm =
    let len = ref 0 in
    let i = ref 0 in
    Stream.from (fun _ ->
        if !i >= !len then (
          len := Unix.read t buff 0 (Bytes.length buff);
          i := 0;
          if !len > 0 then output oc buff 0 !len);
        if !len = 0 then None
        else (
          incr i;
          Some (Bytes.get buff (!i - 1))))
  in
  let _ = get_request_and_content strm in
  ()

let rec list_remove x = function
  | [] -> failwith "list_remove"
  | y :: l -> if x = y then l else y :: list_remove x l

let pids = ref []

(* If there is a maximum number of forks, this function waits until
   the number of active forks is under this limit. *)
let wait_available max_clients socket =
  match max_clients with
  | Some m ->
      if List.length !pids >= m then ignore @@ loop_wait ();
      let stop_verbose = ref false in
      while !pids <> [] && (not @@ is_data_available ~timeout:15. socket) do
        if not !stop_verbose then (
          stop_verbose := true;
          let tm = Unix.localtime (Unix.time ()) in
          Format.eprintf
            "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after \
             cleanup@."
            tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
            tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (List.length !pids))
      done
  | None -> ()

let skip_possible_remaining_chars fd =
  if not !connection_closed then skip_possible_remaining_chars fd

let check_stopping () =
  if Sys.file_exists !stop_server then (
    flush stdout;
    eprintf "\nServer stopped by presence of file %s.\n" !stop_server;
    eprintf "Remove that file to allow servers to run again.\n";
    flush stderr;
    exit 0)

let client_connection tmout callback addr t =
  let oc = Unix.out_channel_of_descr t in
  wserver_oc := oc;
  Fun.protect ~finally:close_connection @@ fun () ->
  treat_connection_with_timeout ~timeout:tmout callback addr t

let accept_connection tmout max_clients callback s =
  let () = wait_available max_clients s in
  let t, addr = loop_accept s in
  connection_closed := false;
  wserver_sock := t;
  check_stopping ();
  Unix.setsockopt t Unix.SO_KEEPALIVE true;
  Fun.protect
    ~finally:(fun () ->
      if not !connection_closed then (
        connection_closed := true;
        Unix.close t))
    (fun () ->
      if Sys.unix then
        if !no_fork then client_connection tmout callback addr t
        else
          match Unix.fork () with
          | exception _ ->
              Format.eprintf "Fork failed@."
          | 0 -> (
              try
                Unix.close s;
                client_connection tmout callback addr t;
                exit 0
              with Unix.Unix_error (Unix.ECONNRESET, "read", _) -> exit 0)
          | pid ->
              Format.eprintf "Fork %d@." pid;
              pids := pid :: !pids
      else (
        Compat.Out_channel.with_open_bin !sock_in (fun oc ->
            try copy_what_necessary t oc with Unix.Unix_error _ -> ());
        let pid =
          let env =
            Array.append (Unix.environment ())
              [| "WSERVER=" ^ string_of_sockaddr addr |]
          in
          let args = Sys.argv in
          Unix.create_process_env Sys.argv.(0) args env Unix.stdin Unix.stdout
            Unix.stderr
        in
        let _ = Unix.waitpid [] pid in
        Compat.In_channel.with_open_bin !sock_in close_in;
        let shutdown () =
          (try Unix.shutdown t Unix.SHUTDOWN_SEND with _ -> ());
          skip_possible_remaining_chars t;
          try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with _ -> ()
        in
        Fun.protect ~finally:shutdown (fun () ->
            try
              Compat.In_channel.with_open_bin !sock_out (fun ic ->
                  try
                    let rec loop () =
                      let len = input ic buff 0 (Bytes.length buff) in
                      if len = 0 then ()
                      else (
                        (let rec loop_write i =
                           let olen = Unix.write t buff i (len - i) in
                           if i + olen < len then loop_write (i + olen)
                         in
                         loop_write 0);
                        loop ())
                    in
                    loop ()
                  with Unix.Unix_error _ -> ())
            with Unix.Unix_error _ ->
              (* A Unix exception could be raised by [In_channel.open_bin]
                 inside [In_channel.with_open_bin]. *)
              ())))

let reap_children =
  let wait_any_child () = Unix.waitpid [ Unix.WNOHANG ] (-1) |> fst in
  fun (_ : int) ->
    (* This code follows Section 26.3.1 of `The Linux Programming Interface`
       by Michael Kerrisk. It prevents zombie processes by repeadly waiting for
       terminated children until none remain.

       A loop is necessary, as multiple children may terminate while the master
       process is executing this handler and the SIGCHLD signal of these children
       could be ignored. *)
    try
      while true do
        match wait_any_child () with
        | 0 | (exception Unix.Unix_error (Unix.ECHILD, "waitpid", "")) ->
            raise_notrace Exit
        | pid -> pids := list_remove pid !pids
      done
    with Exit -> ()

let f syslog addr_opt port tmout max_clients g =
  if Sys.unix && not !no_fork then
    Sys.set_signal Sys.sigchld (Signal_handle reap_children);
  match
    if Sys.unix then None
    else try Some (Sys.getenv "WSERVER") with Not_found -> None
  with
  | Some s ->
      let addr = sockaddr_of_string s in
      let fd = Unix.openfile !sock_in [ Unix.O_RDONLY ] 0 in
      let oc = open_out_bin !sock_out in
      wserver_oc := oc;
      ignore (treat_connection_with_timeout ~timeout:tmout g addr fd);
      exit 0
  | None ->
      check_stopping ();
      let addr =
        match addr_opt with
        | None ->
            if Unix.string_of_inet_addr Unix.inet6_addr_any = "::" then
              Unix.inet_addr_any
            else Unix.inet6_addr_any
        | Some addr -> (
            try Unix.inet_addr_of_string addr
            with Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0))
      in
      let s =
        Unix.socket
          (if Unix.string_of_inet_addr Unix.inet6_addr_any = "::" then
           Unix.PF_INET
          else Unix.PF_INET6)
          Unix.SOCK_STREAM 0
      in
      if Unix.string_of_inet_addr Unix.inet6_addr_any <> "::" then
        Unix.setsockopt s Unix.IPV6_ONLY false;
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (addr, port));
      Unix.listen s 4;
      let tm = Unix.localtime (Unix.time ()) in
      eprintf "Ready %4d-%02d-%02d %02d:%02d port %d...\n"
        (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min port;
      flush stderr;
      if !no_fork then ignore @@ Sys.signal Sys.sigpipe Sys.Signal_ignore;
      while true do
        try accept_connection tmout max_clients g s with
        | Unix.Unix_error (Unix.ECONNRESET, "accept", _) as e ->
            syslog `LOG_INFO (Printexc.to_string e)
        | Sys_error msg as e when msg = "Broken pipe" ->
            syslog `LOG_INFO (Printexc.to_string e)
      done
