module Y = Yojson.Safe
module U = Yojson.Safe.Util
module Database = Geneweb_db.Database
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module GWPARAM = Geneweb.GWPARAM

let ( let* ) = Result.bind
let error ppf = Fmt.kstr (fun s -> Error s) ppf

let run path args =
  let args =
    Array.init
      (Array.length args + 1)
      (fun i -> if i = 0 then path else args.(i - 1))
  in
  let ((stdout, stdin, stderr) as channels) =
    Unix.open_process_args_full path args (Unix.environment ())
  in
  let r, err = (In_channel.input_all stdout, In_channel.input_all stderr) in
  match Unix.close_process_full channels with
  | WEXITED 0 -> Ok r
  | _ -> error "Processus %S failed with error:\n %s" path err

let curl url = run "curl" [| "-o"; "/dev/null"; "-w"; "%{json}"; url |]

type request = { name : string; pattern : string }

let parse_request json =
  match (U.member "name" json, U.member "pattern" json) with
  | `String name, `String pattern -> Ok { name; pattern }
  | _ -> error "Malformed request"

let parse_requests json =
  match U.member "requests" json with
  | `List l ->
      List.fold_left
        (fun acc json ->
          let* request = parse_request json in
          let* acc = acc in
          Ok (request :: acc))
        (Ok []) l
  | _ -> error "No requests field"

let parse_input f =
  let json = In_channel.with_open_text f Y.from_channel in
  parse_requests json

let extract_result json =
  let extract_string s json =
    match U.member s json with
    | `String s -> Ok s
    | _ | (exception U.Type_error _) -> error "Malformed curl response"
  in
  let extract_int s json =
    match U.member s json with
    | `Int i -> Ok i
    | _ | (exception U.Type_error _) -> error "Malformed curl response"
  in
  let extract_float s json =
    match U.member s json with
    | `Float f -> Ok f
    | _ | (exception U.Type_error _) -> error "Malformed curl response"
  in
  let* url = extract_string "url_effective" json in
  let* code = extract_int "response_code" json in
  let* time_pretransfer = extract_float "time_pretransfer" json in
  let* time_total = extract_float "time_total" json in
  Ok (url, code, time_pretransfer, time_total)

let analyze_result ~name json =
  let* url, code, time_pretransfer, time_total = extract_result json in
  Ok
    (`Assoc
       [
         ("name", `String name);
         ("url", `String url);
         ("code", `Int code);
         ("time_pretransfer", `Float time_pretransfer);
         ("time_total", `Float time_total);
       ])

type var = { regexp : Str.regexp; value : string }

module M = Map.Make (struct
  type t = Str.regexp

  let compare = compare
end)

let fn_var = Str.regexp ".*\\$FN.*"
let sn_var = Str.regexp ".*\\$SN.*"
let occ_var = Str.regexp ".*\\$OC.*"
let variables = [ fn_var; sn_var; occ_var ]

let load_database bname =
  GWPARAM.init ();
  let bname = !GWPARAM.bpath bname in
  Driver.with_database bname @@ fun base -> Driver.load_persons_array base

let generate_map bname =
  let bname = !GWPARAM.bpath bname in
  Driver.with_database bname @@ fun base ->
  let ipers = Driver.ipers base in
  let it = Collection.iterator ipers in
  let rec loop () =
    let x = Random.int_in_range ~min:0 ~max:100_000 in
    match it () with
    | Some iper when x > 75_000 -> iper
    | Some _ -> loop ()
    | None -> assert false
  in
  let iper = loop () in
  let per = Driver.poi base iper in
  let fn = Driver.p_first_name base per in
  let sn = Driver.p_surname base per in
  let occ = Driver.get_occ per in
  (fn, sn, occ)

let produce_url map pat =
  let e = M.fold Str.global_replace map pat in
  Fmt.str "http://127.0.0.1:2317/roglo-v7?%s" e

let bench_request map request =
  let* output = curl @@ produce_url map request.pattern in
  match Y.from_string output with
  | exception Yojson.Json_error e -> Error e
  | json ->
      let* r = analyze_result ~name:request.name json in
      Ok (Y.pretty_to_string ~std:true r)

module Cmd = struct
  module C = Cmdliner

  type t = {
    input : string;
    database : string;
    output : string option;
    seed : int option;
  }

  let seed =
    let doc =
      "Choose the seed the select ramdomly people, places, titles and so on in \
       the base."
    in
    C.Arg.(value & opt (some int) None & info [ "s"; "seed" ] ~doc ~docv:"INT")

  let output =
    let doc = "Output file" in
    C.Arg.(value & opt (some filepath) None & info [ "o"; "output" ] ~doc)

  let input =
    let doc =
      "Choose the seed the select ramdomly people, places, titles and so on in \
       the base."
    in
    C.Arg.(required & pos 0 (some filepath) None & info [] ~doc ~docv:"INPUT")

  let database =
    let doc = "Use $(docv) as source." in
    C.Arg.(
      required & pos 1 (some filepath) None & info [] ~docv:"DATABASE" ~doc)

  let t =
    let open Cmdliner.Term.Syntax in
    let doc = "" in
    C.Cmd.make (C.Cmd.info "bench-request" ~doc)
    @@
    let+ input = input
    and+ database = database
    and+ output = output
    and+ seed = seed in
    { input; database; output; seed }
end

let initialize ?seed () =
  match seed with None -> Random.self_init () | Some s -> Random.init s

let () =
  match Cmdliner.Cmd.eval_value' Cmd.t with
  | `Ok o -> (
      match parse_input o.input with
      | Error e -> Fmt.epr "Parsing error of the input: %s@." e
      | Ok rs ->
          initialize ?seed:o.seed ();
          Format.printf "Loading database...@.";
          load_database o.database;
          Format.printf "Generating input...@.";
          let fn, sn, occ = generate_map o.database in
          let map =
            M.of_list
              [ (fn_var, fn); (sn_var, sn); (occ_var, string_of_int occ) ]
          in
          List.iter
            (fun r ->
              match bench_request map r with
              | Error e -> Fmt.epr "Failed: %s@." e
              | Ok s -> Fmt.pr "%s@." s)
            rs)
  | `Exit code -> exit code
