module type S = sig
  type t
  type word

  val empty : t
  val add : word -> word -> t -> t
  val search : word list -> t -> word list
end

let take k l =
  let rec loop i l =
    match l with
    | [] -> []
    | x :: xs ->
      if i = k then []
      else x :: loop (i + 1) xs
  in
  loop 0 l

module Make (W : Word.S) = struct
  module T = Trie.Make (W)
  module WS = Set.Make (W)

  type word = W.t
  type t = WS.t T.t

  let empty = T.empty

  let add w s t =
    T.update w
      (function
        | Some set -> Some (WS.add s set) | None -> Some (WS.singleton s))
      t

  let search words t =
    let tbl : (W.t, int) Hashtbl.t = Hashtbl.create 17 in
    List.iter
      (fun word ->
        Seq.iter
          (fun (_, set) ->
            WS.iter
              (fun s ->
                let i =
                  match Hashtbl.find tbl s with
                  | exception Not_found -> 0
                  | i -> i
                in
                Hashtbl.replace tbl s (i + 1))
              set)
          (T.search word t))
      words;
    Hashtbl.to_seq tbl |> List.of_seq
    |> List.sort (fun (_, i) (_, j) -> Int.compare j i)
    |> List.map fst
    |> take 10
end

module Default = Make (Word.Default)
