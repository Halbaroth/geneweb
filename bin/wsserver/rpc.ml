let of_payload id payload =
  `Assoc [ ("id", `Int id); ("error", `Null); ("payload", payload) ]

let of_error id s =
  `Assoc [ ("id", `Int id); ("error", `String s); ("payload", `Null) ]
  |> Server.of_json |> Lwt.return

let of_string id s = of_payload id (`String s)

let pong id =
  let json = of_string id "pong" in
  Lwt.return @@ Server.of_json json

let search_index id idx pattern =
  let json = of_string id "plop" in
  Lwt.return @@ Server.of_json json

(* type 'a basic =
  | Int : int -> int basic
  | Float : float -> float basic

type _ proc =
  | Basic : 'a basic -> 'a proc
  | Arrow : 'a basic * 'b proc -> ('a -> 'b) proc *)

let dispatch sockaddr Server.{ content; _ } =
  let open Yojson.Safe.Util in
  (* TODO: do validation to simply this. *)
  let id = content |> member "id" |> to_int in
  let procedure = content |> member "procedure" |> to_string_option in
  match procedure with
  | Some "ping" -> pong id
  | Some "search_index" ->
    let args = content |> member "args" |> to_list in
    let idx = List.nth args 0 |> to_string in
    let pattern = List.nth args 1 |> to_string in
    search_index id idx pattern
  | Some _ | None -> of_error id "unknown remote procedure"
