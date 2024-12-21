module A = Alcotest
module Iterator = Geneweb_structures.Iterator

module I = struct
  type elt = int
  type witness

  let compare = Int.compare
  let dummy = 0
end

module Iset = Geneweb_structures.Iset.Make (I)

module Naive = struct
  module SI = Set.Make (Int)

  type elt = int
  type cmp = I.witness
  type t = SI.t

  let of_seq = SI.of_seq
  let to_seq = SI.to_seq
  let mem = SI.mem
  let cardinal = SI.cardinal
  let empty = SI.empty
  let inter = SI.inter
  let union = SI.union

  let iterator t =
    (module struct
      type elt = int
      type nonrec cmp = cmp

      let state = ref (SI.to_seq t)

      let curr () =
        match !state () with
        | Seq.Nil -> raise Iterator.End
        | Seq.Cons (hd, _) -> hd

      let next () =
        match !state () with Seq.Nil -> () | Seq.Cons (_, tl) -> state := tl

      let seek w =
        let rec loop () =
          match curr () with
          | exception Iterator.End -> ()
          | v when w <= v -> ()
          | _ ->
              next ();
              loop ()
        in
        loop ()
    end : Iterator.S with type elt = int and type cmp = cmp)
end

let nonempty_array = QCheck.Gen.(array_size (int_range 1 100) int)

let index_array =
  QCheck.Gen.(
    nonempty_array >>= fun a ->
    int_range 0 (Array.length a - 1) >>= fun i -> pure (a, i))

let test_empty () =
  let s = Iset.of_seq Seq.empty in
  let it = Iset.iterator s in
  Iterator.next it;
  Iterator.seek 10 it;
  let b =
    match Iterator.curr it with
    | exception Iterator.End -> true
    | _ -> false
  in
  A.(check bool) "end iterator" true b

let test_seek_advance () =
  let s = Iset.of_seq (List.to_seq [ 1; 3; 5; 9 ]) in
  let it = Iset.iterator s in
  Iterator.seek 4 it;
  A.(check int) "first seek" 5 (Iterator.curr it);
  Iterator.seek 4 it;
  A.(check int) "second seek" 5 (Iterator.curr it)

let test_random_mem =
  QCheck.Test.make ~count:1000 ~name:"random mem" (QCheck.make index_array)
  @@ fun (a, i) ->
  let seq = Array.to_seq a in
  let s1 = Naive.of_seq seq in
  let s2 = Iset.of_seq seq in
  Naive.mem a.(i) s1 = Iset.mem a.(i) s2

let test_random_iterator_next =
  QCheck.Test.make ~count:1000 ~name:"random iterator next"
    (QCheck.make nonempty_array)
  @@ fun a ->
  let seq = Array.to_seq a in
  let it1 = Naive.iterator @@ Naive.of_seq seq in
  let it2 = Iset.iterator @@ Iset.of_seq seq in
  Iterator.equal (module I) it1 it2

let test_random_iterator_seek =
  QCheck.Test.make ~count:1000 ~name:"random iterator seek"
    (QCheck.make index_array)
  @@ fun (a, i) ->
  let seq = Array.to_seq a in
  let it1 = Naive.iterator @@ Naive.of_seq seq in
  let it2 = Iset.iterator @@ Iset.of_seq seq in
  let probe = a.(i) + 5 in
  Iterator.seek probe it1;
  Iterator.seek probe it2;
  let v1 =
    try Some (Iterator.curr it1) with Iterator.End -> None
  in
  let v2 = try Some (Iterator.curr it2) with Iterator.End -> None in
  Option.equal Int.equal v1 v2

let test_random_iterator_union =
  QCheck.Test.make ~count:1000 ~name:"random iterator union"
    QCheck.(make Gen.(list_size (int_range 1 100) nonempty_array))
  @@ fun l ->
  let it1 =
    let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
    List.fold_left Naive.union Naive.empty l1 |> Naive.iterator
  in
  let it2 =
    let l2 =
      List.map (fun a -> Array.to_seq a |> Iset.of_seq |> Iset.iterator) l
    in
    Iterator.union (module I) l2
  in
  Iterator.equal (module I) it1 it2

let test_random_iterator_join =
  QCheck.Test.make ~count:1000 ~name:"random iterator join"
    QCheck.(make Gen.(list_size (int_range 1 100) nonempty_array))
  @@ fun l ->
  let it1 =
    let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
    let x = List.hd l1 in
    List.fold_left Naive.inter x l1 |> Naive.iterator
  in
  let it2 =
    let l2 =
      List.map (fun a -> Array.to_seq a |> Iset.of_seq |> Iset.iterator) l
    in
    Iterator.join (module I) l2
  in
  Iterator.equal (module I) it1 it2

let () =
  let quick_test s = A.test_case s `Quick in
  let qcheck_test = QCheck_alcotest.to_alcotest in
  A.run __FILE__
    [
      ( "corner cases",
        [
          quick_test "empty" test_empty;
          quick_test "seek advance" test_seek_advance;
        ] );
      ( "random set operations",
        [
          qcheck_test test_random_mem;
          qcheck_test test_random_iterator_next;
          qcheck_test test_random_iterator_seek;
          qcheck_test test_random_iterator_union;
          qcheck_test test_random_iterator_join;
        ] );
    ]
