module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Compat = Geneweb_compat

let ( // ) = Filename.concat

let with_timer f =
  let start = Unix.gettimeofday () in
  let r = f () in
  let stop = Unix.gettimeofday () in
  (r, stop -. start)

let with_bar ~total =
  let bar ~total =
    Progress.Line.(
      list [ spinner (); bar total; count_to total; percentage_of total ])
  in
  Progress.with_reporter (bar ~total)

module Table = Driver.Iper.Table

let process_person base iper =
  let tbl : Z.t Table.t = Table.create 17 in
  let rec loop iper =
    match Table.find tbl iper with
    | v -> v
    | exception Not_found ->
        let p = Driver.poi base iper in
        let asc = Driver.gen_ascend_of_person p in
        let r =
          match asc.Def.parents with
          | Some ifam ->
              let f = Driver.foi base ifam in
              let ifather = Driver.get_father f in
              let imother = Driver.get_mother f in
              let u =
                match Table.find tbl ifather with
                | _ -> Z.zero
                | exception Not_found -> loop ifather
              in
              let v =
                match Table.find tbl imother with
                | _ -> Z.zero
                | exception Not_found -> loop imother
              in
              Z.(one + u + v)
          | None -> Z.one
        in
        Table.add tbl iper r;
        r
  in
  loop iper

module Pool : sig
  type t

  val create : int -> t
  val spawn : t -> (unit -> 'a) -> 'a Domain.t
end = struct
  type t = Semaphore.Counting.t

  let create = Semaphore.Counting.make

  let spawn t f =
    Semaphore.Counting.acquire t;
    let finally () = try Semaphore.Counting.release t with Sys_error _ -> () in
    Domain.spawn @@ fun () ->
    Fun.protect ~finally f
end

let process_base ~in_memory ~jobs bname =
  let bfile = Secure.base_dir () // bname in
  if in_memory then (
    Logs.app (fun k -> k "Loading %S in memory..." bname);
    Driver.load_database bfile);
  Driver.with_database bfile @@ fun base ->
  let ipers = Driver.ipers base in
  let total = Driver.nb_of_persons base in
  let pool = Pool.create jobs in
  with_bar ~total @@ fun f ->
  Collection.fold
    (fun acc iper ->
      let r = Pool.spawn pool (fun () -> process_person base iper) in
      let p = Driver.poi base iper in
      let fn = Driver.get_first_name p |> Driver.sou base in
      let sn = Driver.get_surname p |> Driver.sou base in
      f 1;
      (iper, fn, sn, r) :: acc)
    [] ipers

let write_output ~bname r =
  let fl = Filename.temp_file "geneweb" bname in
  Logs.app (fun k -> k "Writing results in %S" fl);
  Out_channel.with_open_text fl @@ fun oc ->
  List.iter
    (fun (iper, fn, sn, job) ->
      let c = Domain.join job in
      let s =
        Fmt.str "%a(%S, %S): %a\n" Driver.Iper.pp iper fn sn Z.pp_print c
      in
      Out_channel.output_string oc s)
    r

let run ~bd ~debug ~in_memory ~jobs bases =
  if debug then Logs.set_level ~all:true (Some Logs.Debug);
  Secure.set_base_dir bd;
  let (), duration =
    with_timer @@ fun () ->
    List.iter
      (fun bname ->
        let r = process_base ~in_memory ~jobs bname in
        write_output ~bname r)
      bases
  in
  Logs.app (fun k -> k "Total time: %6.2fs" duration)

module Cmd = struct
  open Cmdliner
  open Cmdliner.Term.Syntax

  let bd =
    let doc = "Base directory" in
    Arg.(value & opt string "." & info [ "bd" ] ~docv:"PATH" ~doc)

  let bases =
    let doc = "Base name" in
    Arg.(value & opt_all string [] & info [ "b"; "base" ] ~docv:"BASE" ~doc)

  let debug =
    let doc = "Debug flag" in
    Arg.(value & flag & info [ "debug" ] ~doc)

  let in_memory =
    let doc = "Load data in memory" in
    Arg.(value & flag & info [ "in-memory" ] ~doc)

  let jobs =
    let doc = "Number of parallel jobs" in
    Arg.(
      value
      & opt int (Domain.recommended_domain_count ())
      & info [ "j"; "jobs" ] ~docv:"NUM" ~doc)

  let t =
    let doc = "Compute the number of implexes" in
    Cmd.v (Cmd.info "Foo" ~doc)
    @@ let+ bd = bd
       and+ debug = debug
       and+ in_memory = in_memory
       and+ bases = bases
       and+ jobs = jobs in
       run ~bd ~debug ~in_memory ~jobs bases
end

let () =
  Fmt.set_style_renderer Format.err_formatter `Ansi_tty;
  let pp_header _ _ = () in
  Logs.set_reporter (Progress.logs_reporter ~pp_header ());
  exit @@ Cmdliner.Cmd.eval Cmd.t
