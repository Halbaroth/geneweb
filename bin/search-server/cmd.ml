module C = Cmdliner

type dflag = Server | TLS

type cfg = {
  interface : string;
  port : int;
  base_dir : string;
  index_dir : string;
  tls : (string * string) option;
  dflags : dflag list;
}

let default_address = "localhost"
let default_port = 8080
let default_tls_port = 8443
let default_base_dir = "bases"
let default_index_dir = "bases"

module Dflag = struct
  let all = [ Server; TLS ]
  let show = function Server -> "server" | TLS -> "tls"

  let t =
    let enum_conv = List.map (fun v -> (show v, v)) all |> C.Arg.enum in
    let doc =
      Fmt.str "Set the debugging flags, $(docv) must be %s."
        (C.Arg.doc_alts (List.map show all))
    in
    C.Arg.(
      value & opt_all enum_conv [] & info [ "d"; "debug" ] ~docv:"DEBUG" ~doc)
end

module Connection = struct
  let interface =
    let doc = "Listen on the interface" in
    C.Arg.(
      value & opt string default_address
      & info [ "i"; "interface" ] ~docv:"INTERFACE" ~doc)

  let port =
    let doc =
      Fmt.str
        "Listen on the port.The port %d is the default without TLS and %d is \
         the default with TLS"
        default_port default_tls_port
    in
    C.Arg.(value & opt (some int) None & info [ "p"; "port" ] ~docv:"PORT" ~doc)

  let crt =
    let doc = "Certificate" in
    C.Arg.(
      value & opt (some string) None & info [ "c"; "crt" ] ~docv:"CERT" ~doc)

  let key =
    let doc = "Private key" in
    C.Arg.(
      value & opt (some string) None & info [ "k"; "key" ] ~docv:"KEY" ~doc)

  let parse_connection_opt interface port crt key =
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
    (interface, port, tls)

  let t = C.Term.(const parse_connection_opt $ interface $ port $ crt $ key)
end

let base_dir_t =
  let doc = "Base directory" in
  C.Arg.(
    value
    & opt string default_base_dir
    & info [ "b"; "base-dir" ] ~docv:"DIR" ~doc)

let index_dir_t =
  let doc = "Index directory" in
  C.Arg.(
    value
    & opt string default_index_dir
    & info [ "idx"; "index-dir" ] ~docv:"DIR" ~doc)

let mk_cfg (interface, port, tls) base_dir index_dir dflags =
  { interface; port; tls; base_dir; index_dir; dflags }

let parse () =
  let doc = "Server" in
  let info = C.Cmd.info "prototype-server" ~version:"dev" ~doc in
  let cmd =
    C.Cmd.v info
      C.Term.(const mk_cfg $ Connection.t $ base_dir_t $ index_dir_t $ Dflag.t)
  in
  match C.Cmd.eval_value cmd with
  | Ok (`Ok cfg) -> cfg
  | Ok (`Version | `Help) -> exit 0
  | Error `Parse -> exit 124
  | Error `Exn -> exit 125
  | Error `Term -> exit 1
