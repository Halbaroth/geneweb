module A = Alcotest
module Iset = Geneweb_structures.Iset.Make (Int)
module SI = Set.Make (Int)

let nonempty_array = QCheck.Gen.(array_size (int_range 1 100) int)

let index_array =
  QCheck.Gen.(
    nonempty_array >>= fun a ->
    int_range 0 (Array.length a - 1) >>= fun i -> pure (a, i))

let test_empty () =
  let s = Iset.of_seq Seq.empty in
  let it = Iset.iterator s in
  Iset.Iterator.next it;
  Iset.Iterator.seek 10 it;
  let b =
    match Iset.Iterator.curr it with
    | exception Iset.Iterator.End -> true
    | _ -> false
  in
  A.(check bool) "end iterator" true b

let test_seek_advance () =
  let s = Iset.of_seq (List.to_seq [ 1; 3; 5; 9 ]) in
  let it = Iset.iterator s in
  Iset.Iterator.seek 4 it;
  A.(check int) "first seek" 5 (Iset.Iterator.curr it);
  Iset.Iterator.seek 4 it;
  A.(check int) "second seek" 5 (Iset.Iterator.curr it)

let test_random_mem =
  QCheck.Test.make ~count:1000 ~name:"random mem" (QCheck.make index_array)
  @@ fun (a, i) ->
  let seq = Array.to_seq a in
  let s1 = SI.of_seq seq in
  let s2 = Iset.of_seq seq in
  SI.mem a.(i) s1 = Iset.mem a.(i) s2

let test_random_iterator_next =
  QCheck.Test.make ~count:1000 ~name:"random iterator next"
    (QCheck.make nonempty_array)
  @@ fun a ->
  let seq = Array.to_seq a in
  let s1 = SI.of_seq seq in
  let s2 = Iset.of_seq seq in
  let it = Iset.iterator s2 in
  let rec loop acc =
    match Iset.Iterator.curr it with
    | exception Iset.Iterator.End -> acc
    | v ->
        Iset.Iterator.next it;
        loop (v :: acc)
  in
  let l1 = SI.to_seq s1 |> List.of_seq in
  let l2 = loop [] |> List.rev in
  List.equal Int.equal l1 l2

let naive_seek x = SI.find_first (fun e -> Int.compare e x >= 0)

let test_random_iterator_seek =
  QCheck.Test.make ~count:1000 ~name:"random iterator seek"
    (QCheck.make index_array)
  @@ fun (a, i) ->
  let seq = Array.to_seq a in
  let s1 = SI.of_seq seq in
  let s2 = Iset.of_seq seq in
  let it = Iset.iterator s2 in
  let probe = a.(i) + 5 in
  Iset.Iterator.seek probe it;
  let v1 =
    match naive_seek probe s1 with exception Not_found -> None | v -> Some v
  in
  let v2 =
    match Iset.Iterator.curr it with
    | exception Iset.Iterator.End -> None
    | v -> Some v
  in
  Option.equal Int.equal v1 v2

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
        ] );
    ]
