exception File_error of string

let raise_error ppf = Fmt.kstr (fun s -> raise (File_error s)) ppf

let with_in_channel path f =
  let ic = In_channel.open_text path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () -> f ic

let with_out_channel path f =
  let oc = Out_channel.open_text path in
  Fun.protect ~finally:(fun () -> close_out oc) @@ fun () -> f oc

let check_perm path perm =
  let Unix.{ st_perm; _ } = Unix.stat path in
  st_perm land perm = perm

let check_kind ~kind path =
  let Unix.{ st_kind; _ } = Unix.stat path in
  match kind with `File -> st_kind = Unix.S_REG | `Dir -> st_kind = Unix.S_DIR

let create_file ?(required_perm = 0o644) path =
  if Sys.file_exists path then (
    if not @@ check_perm path required_perm then
      raise_error "%s exists but it has not the required permission %o" path
        required_perm;
    if not @@ check_kind ~kind:`File path then
      raise_error "%s exists but it is not a regular file" path)
  else Unix.openfile path [ Unix.O_CREAT ] required_perm |> Unix.close

let create_dir ?(required_perm = 0o700) path =
  if Sys.file_exists path then (
    if not @@ check_perm path required_perm then
      raise_error "%s exists but it has not the required permission %o" path
        required_perm;
    if not @@ check_kind ~kind:`Dir path then
      raise_error "%s exists but it is not a directory" path)
  else Unix.mkdir path required_perm

let walk_folder ?(recursive = false) f path acc =
  let rec walk_siblings dirs path handle acc =
    match Unix.readdir handle with
    | exception End_of_file -> (dirs, acc)
    | "." | ".." -> walk_siblings dirs path handle acc
    | s -> (
        let fl = Filename.concat path s in
        let stat = Unix.stat fl in
        match stat.st_kind with
        | Unix.S_REG -> walk_siblings dirs path handle (f (`File fl) acc)
        | Unix.S_DIR ->
            let dirs = if recursive then fl :: dirs else dirs in
            walk_siblings dirs path handle (f (`Dir fl) acc)
        | _ -> walk_siblings dirs path handle acc)
  in
  let rec traverse stack acc =
    match stack with
    | [] -> acc
    | path :: stack ->
        let stack, acc =
          let handle = Unix.opendir path in
          Fun.protect ~finally:(fun () -> Unix.closedir handle) @@ fun () ->
          walk_siblings stack path handle acc
        in
        traverse stack acc
  in
  traverse [ path ] acc
