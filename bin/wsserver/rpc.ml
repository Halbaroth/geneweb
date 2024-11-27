type error = [ `Wrong_json ]

let of_payload id payload =
  `Assoc [ ("id", `Int id); ("error", `Null); ("payload", payload) ]

let of_string id s = of_payload id (`String s)

let pong id =
  let json = of_string id "pong" in
  Lwt_result.ok @@ Lwt.return @@ Server.of_json json

let dispatch sockaddr Server.{ content; _ } =
  let open Yojson.Safe.Util in
  (* TODO: do validation to simply this. *)
  let id = content |> member "id" |> to_int_option in
  let procedure = content |> member "procedure" |> to_string_option in
  let%lwt response =
    match (procedure, id) with
    | Some "ping", Some id -> pong id
    | _ -> Lwt_result.error @@ Lwt.return "NOO"
  in
  Lwt.return response
