module I = Geneweb_search.Index.Default

module Context = struct
  type t = { word : string; offset : int; len : int }

  let to_json t =
    `Assoc
      [
        ("word", `String t.word); ("offset", `Int t.offset); ("len", `Int t.len);
      ]

  let compare { word = c1; offset = o1; len = l1 }
      { word = c2; offset = o2; len = l2 } =
    let c = String.compare c1 c2 in
    if c <> 0 then c
    else
      let c = Int.compare o1 o2 in
      if c <> 0 then c else Int.compare l1 l2

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

let of_payload id payload =
  `Assoc [ ("id", `Int id); ("error", `Null); ("payload", payload) ]
  |> Server.of_json |> Lwt.return

let of_error id s =
  `Assoc [ ("id", `Int id); ("error", `String s); ("payload", `Null) ]
  |> Server.of_json |> Lwt.return

let of_string id s = of_payload id (`String s)
let of_list id l = of_payload id (`List l)
let pong id = of_string id "pong"
let is_separator c = c = ' ' || c = ',' || c = '-'

let flush_buf acc buf start curr =
  match buf with [] -> acc | _ -> (List.rev buf, start, curr) :: acc

let tokenize s =
  let len = String.length s in
  let rec loop acc buf start curr =
    if curr = len then flush_buf acc buf start curr
    else
      let c = String.get s curr in
      if is_separator c then
        let acc = flush_buf acc buf start curr in
        loop acc [] (curr + 1) (curr + 1)
      else loop acc (c :: buf) start (curr + 1)
  in
  loop [] [] 0 0

let preprocess s =
  tokenize s |> List.rev
  |> List.map (fun (tk, _, _) ->
         List.to_seq tk |> String.of_seq |> Util.normalize)

let search_index indexes id idx input =
  match Util.MS.find idx indexes with
  | exception Not_found -> of_error id "unknown index"
  | idx ->
      let pats = preprocess input in
      let seq =
        Seq.concat
        @@ Seq.map
             (fun pat -> Seq.take 4 @@ I.search pat idx)
             (List.to_seq pats)
      in
      let l =
        Seq.concat
        @@ Seq.map (fun (_, v) -> Seq.take 4 @@ Context.Set.to_seq v) seq
        |> List.of_seq
      in
      l |> List.map Context.to_json |> of_list id

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
