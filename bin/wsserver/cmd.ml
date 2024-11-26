open Cmdliner

type debug_flag = Internal | Tls

type cfg = {
  host : string;
  port : int;
  base_dir : string;
  tls : (string * string) option;
  debug_flags : debug_flag list;
}

let default_address = "localhost"
let default_port = 8080
let default_tls_port = 8443
let default_base_dir = "bases"

module Debug_flag = struct
  let all = [ Internal; Tls ]
  let show = function Internal -> "internal" | Tls -> "tls"

  let t =
    let enum_conv = List.map (fun v -> (show v, v)) all |> Arg.enum in
    let doc =
      Fmt.str "Set the debugging flags, $(docv) must be %s."
        (Arg.doc_alts (List.map show all))
    in
    Arg.(
      value & opt_all enum_conv [] & info [ "d"; "debug" ] ~docv:"DEBUG" ~doc)
end

module Connection = struct
  let host =
    let doc = "Listen on the address" in
    Arg.(
      value & opt string default_address
      & info [ "h"; "host" ] ~docv:"HOST" ~doc)

  let port =
    let doc =
      Fmt.str
        "Listen on the port.The port %d is the default without TLS and %d is \
         the default with TLS"
        default_port default_tls_port
    in
    Arg.(value & opt (some int) None & info [ "p"; "port" ] ~docv:"PORT" ~doc)

  let crt =
    let doc = "Certificate" in
    Arg.(value & opt (some string) None & info [ "c"; "crt" ] ~docv:"CERT" ~doc)

  let key =
    let doc = "Private key" in
    Arg.(value & opt (some string) None & info [ "k"; "key" ] ~docv:"KEY" ~doc)

  (* TODO: add a custom parser to emit an error if the user only specify
     a certificate key or a private key. *)
  let parse_connection_opt host port crt key =
    let port =
      match (port, crt, key) with
      | None, None, None -> default_port
      | None, Some _, Some _ -> default_tls_port
      | None, _, _ -> assert false
      | Some p, _, _ -> p
    in
    let tls =
      match (crt, key) with
      | Some crt, Some key -> Some (crt, key)
      | None, None -> None
      | _ -> assert false
    in
    (host, port, tls)

  let t = Term.(const parse_connection_opt $ host $ port $ crt $ key)
end

let base_dir_t =
  let doc = "Base directory" in
  Arg.(
    value
    & opt string default_base_dir
    & info [ "b"; "base-dir" ] ~docv:"DIR" ~doc)

let mk_cfg (host, port, tls) base_dir debug_flags =
  { host; port; tls; base_dir; debug_flags }

let parse () =
  let doc = "Server" in
  let info = Cmd.info "prototype-server" ~version:"dev" ~doc in
  let cmd =
    Cmd.v info Term.(const mk_cfg $ Connection.t $ base_dir_t $ Debug_flag.t)
  in
  match Cmd.eval_value cmd with
  | Ok (`Ok cfg) -> cfg
  | Ok (`Version | `Help) -> exit 0
  | Error `Parse -> exit 124
  | Error `Exn -> exit 125
  | Error `Term -> (* TODO: check this rc code. *) exit 1
