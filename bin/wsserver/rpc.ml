module I = Geneweb_search.Index.Default
module MS = Map.Make (String)

let of_payload id payload =
  `Assoc [ ("id", `Int id); ("error", `Null); ("payload", payload) ]
  |> Server.of_json |> Lwt.return

let of_error id s =
  `Assoc [ ("id", `Int id); ("error", `String s); ("payload", `Null) ]
  |> Server.of_json |> Lwt.return

let of_string id s = of_payload id (`String s)
let of_list id l = of_payload id (`List l)
let pong id = of_string id "pong"

let search_index indexes id idx pattern =
  match MS.find idx indexes with
  | exception Not_found -> of_error id "unknown index"
  | idx ->
      I.lookup pattern idx |> Seq.take 10 |> List.of_seq
      |> List.map (fun (s, _) -> `String s)
      |> of_list id

let dispatch indexes sockaddr Server.{ content; _ } =
  let open Yojson.Safe.Util in
  let indexes = List.to_seq indexes |> MS.of_seq in
  let search_index = search_index indexes in
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
