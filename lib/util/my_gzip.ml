type in_channel = {
  gic : Gzip.in_channel;
  (* Underlying input channel of camlzip. *)

  buf : Buffer.t;
  (* Buffer used while reading the file. It may contain several new
     lines. It has to be flushed until the last '\n' before
     reading new characters from [gic]. *)

  mutable start : int;
  (* Position in [buf]. Everything before this position has already been
     returned by [input_line]. *)

  mutable eof : bool;
  (* [true] if we have reached the end of the file. *)
}

let[@inline always] buffer_length ic =
  Buffer.length ic.buf - ic.start

let[@inline always] flush_buffer ic =
  Buffer.clear ic.buf;
  ic.start <- 0

let open_in path = {
  gic = Gzip.open_in path;
  buf = Buffer.create 2048;
  start = 0;
  eof = false;
}

let close_in ic =
  Gzip.close_in ic.gic

let close_in_noerr ic =
  Gzip.close_in ic.gic

let with_open path f =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () -> f ic

(* Helper functions *)

let bytes_find_exn f b =
  let len = Bytes.length b in
  let rec loop i =
    if i >= len then raise Not_found
    else if f (Bytes.get b i) then i
    else loop (i + 1)
  in
  loop 0

(* Assume that [ic.buf] does not contain the character [c] before calling
   this function. After calling this function, [buf] can contains several
   times [c].

   Return the number of new character read. *)
let read_until ic c =
  let rec loop acc =
    let b = Bytes.create 500 in
    let r = Gzip.input ic.gic b 0 500 in
    match bytes_find_exn (Char.equal c) b with
    | exception Not_found when r = 0 -> acc
    | exception Not_found ->
        Buffer.add_subbytes ic.buf b 0 r;
        loop (acc + r)
    | _ ->
        Buffer.add_subbytes ic.buf b 0 r;
        acc + r
  in
  loop 0

(* Return the next line contained in the buffer.

   If there is no '\n', raise [Not_found]. We may have
   to read more characters to find the next new line. *)
let next_line ic =
  let loop b =
    let len = Buffer.length b in
    let rec loop i =
      if i >= len then raise Not_found
      else if Char.equal (Buffer.nth b i) '\n' then i
      else loop (i + 1)
    in
    loop ic.start
  in
  let i = loop ic.buf in
  let s = Buffer.sub ic.buf ic.start (i - ic.start) in
  ic.start <- i + 1;
  if buffer_length ic = 0 then flush_buffer ic;
  s

let rec input_line ic =
  match next_line ic with
  | exception Not_found when not ic.eof ->
      (* The buffer does not contain a complete line but we can read
         more characters. *)
      (let r = read_until ic '\n' in
      if r = 0 then ic.eof <- true;
      input_line ic)
  | exception Not_found when buffer_length ic > 0 ->
      (* We have reach the end of the file and there is no more line
         in the buffer. We return all the content of the buffer even
         if there is no newline at the end of the file. *)
      (let s = Buffer.sub ic.buf ic.start (Buffer.length ic.buf) in
      Buffer.clear ic.buf;
      ic.start <- 0;
      s)
  | exception Not_found ->
      assert (buffer_length ic = 0);
      raise End_of_file
  | s -> s
