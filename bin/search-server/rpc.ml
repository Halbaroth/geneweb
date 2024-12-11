module I = Geneweb_search.Index.Default
module Index = Geneweb_search.Index
module Analyze = Geneweb_search.Analyze

(* let to_json (word, ctx) =
   let a =
     List.map
       (fun c ->
         `Assoc [ ("offset", `Int c.Index.offset); ("len", `Int c.Index.len) ])
       ctx
   in
   `Assoc [ ("word", `String word); ("context", `List a) ] *)

let to_json word = `String word

let of_payload id payload =
  `Assoc [ ("id", `Int id); ("error", `Null); ("payload", payload) ]
  |> Server.of_json |> Lwt.return

let of_error id s =
  `Assoc [ ("id", `Int id); ("error", `String s); ("payload", `Null) ]
  |> Server.of_json |> Lwt.return

let of_string id s = of_payload id (`String s)
let of_list id l = of_payload id (`List l)
let pong id = of_string id "pong"

let search_index indexes id idx input =
  match Util.MS.find idx indexes with
  | exception Not_found -> of_error id "unknown index"
  | idx ->
      let pats =
        Analyze.preprocess input |> List.map (fun t -> t.Analyze.content)
      in
      I.search pats idx |> List.map to_json |> of_list id

let dispatch indexes _sockaddr Server.{ content; _ } =
  let open Yojson.Safe.Util in
  let indexes = List.to_seq indexes |> Util.MS.of_seq in
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
