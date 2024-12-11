module I = Geneweb_search.Index.Default
module Index = Geneweb_search.Index

module Context = struct
  type t = { word : string; offset : int; len : int }

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

let to_json (word, ctx) =
  let a =
    List.map
      (fun c ->
        `Assoc [ ("offset", `Int c.Index.offset); ("len", `Int c.Index.len) ])
      ctx
  in
  `Assoc [ ("word", `String word); ("context", `List a) ]

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

let heuristic l =
  let tbl : (string, Index.loc list) Hashtbl.t = Hashtbl.create 17 in
  List.iter
    (fun (s, locs) ->
      match Hashtbl.find tbl s with
      | exception Not_found -> Hashtbl.replace tbl s (List.of_seq locs)
      | l -> Hashtbl.replace tbl s (List.rev_append l (List.of_seq locs)))
    l;
  let l =
    Hashtbl.to_seq tbl
    |> Seq.map (fun (s, locs) ->
           let locs = List.sort_uniq Index.compare_loc locs in
           (s, locs, List.length l))
    |> List.of_seq
  in
  List.sort (fun (_, _, u) (_, _, v) -> Int.compare v u) l
  |> List.map (fun (u, v, _) -> (u, v))
  |> List.to_seq |> Seq.take 10 |> List.of_seq

let search_index indexes id idx input =
  match Util.MS.find idx indexes with
  | exception Not_found -> of_error id "unknown index"
  | idx ->
      let pats = Geneweb_search.Analyze.preprocess input
        |> List.map (fun Index.{ content; _ } -> content)
      in
      let l = I.search2 pats idx in
      heuristic (List.of_seq seq)
      |> List.map to_json
      |> of_list id

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
