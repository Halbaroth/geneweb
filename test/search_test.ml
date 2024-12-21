module A = Alcotest
module T = Geneweb_search.Trie.Default

(* Compute the Levenshtein distance of [s1] and [s2]. *)
let distance s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let l1, l2, s1, s2 = if l2 < l1 then (l2, l1, s2, s1) else (l1, l2, s1, s2) in
  let prev = Array.init (l2 + 1) (fun j -> j) in
  let dp = Array.init (l2 + 1) (fun _ -> 0 (* dummy *)) in
  for i = 1 to l1 do
    dp.(0) <- i;
    for j = 1 to l2 do
      if String.get s1 (i - 1) = String.get s2 (j - 1) then
        dp.(j) <- prev.(j - 1)
      else
        let u = min prev.(j) dp.(j - 1) in
        dp.(j) <- 1 + min u prev.(j - 1)
    done;
    Array.blit dp 0 prev 0 (l2 + 1)
  done;
  prev.(l2)

let test_cardinal _trie _a () =
  let trie =
    T.of_seq @@ List.to_seq [ ("foo", ()); ("bar", ()); ("saucisse", ()) ]
  in
  Fmt.pr "%a@." (T.pp (Fmt.any "()")) trie;
  A.(check int) "cardinal after insertion" 3 (T.cardinal trie)

let test_mem trie _a () =
  A.(check bool) "mem Vieux-Fort" true (T.mem "Vieux-Fort" trie);
  A.(check bool) "mem Vieu" false (T.mem "Vieu" trie);
  A.(check bool) "mem Pariso" false (T.mem "Pariso" trie)

let test_search trie _a () =
  A.(check int) "search Vieux-Fort" 1 (Seq.length @@ T.search "Vieux-Fort" trie)

let test_remove trie _a () =
  A.(check bool) "remove Paris (before)" true (T.mem "Paris" trie);
  let trie = T.remove "Paris" trie in
  A.(check bool) "remove Paris (after)" false (T.mem "Paris" trie)

let test_update trie _a () =
  let c = T.cardinal trie in
  let trie = T.update "Paris" (function Some _ -> None | None -> None) trie in
  A.(check bool) "update Paris" true (T.cardinal trie = c - 1)

let test_lexicographic_order _trie _a () =
  let trie =
    T.of_seq
    @@ List.to_seq
         [ ("abe", ()); ("ab", ()); ("a", ()); ("bcd", ()); ("abcd", ()) ]
  in
  let expected =
    [ ("a", ()); ("ab", ()); ("abcd", ()); ("abe", ()); ("bcd", ()) ]
  in
  A.(check (list (pair string unit)))
    "order of_seq" expected
    (List.of_seq @@ T.to_seq trie);
  let l = T.fold (fun w () acc -> (w, ()) :: acc) trie [] |> List.rev in
  A.(check (list (pair string unit))) "order fold" expected l

let test_random_mem trie a =
  let sz = Array.length a in
  QCheck.Test.make ~count:1000 ~name:"random mem" QCheck.(int_bound (sz - 1))
  @@ fun i -> T.mem a.(i) trie

let test_random_search trie a =
  let sz = Array.length a in
  QCheck.Test.make ~count:1000 ~name:"random search" QCheck.(int_bound (sz - 1))
  @@ fun i -> not @@ Seq.is_empty @@ T.search a.(i) trie

let test_random_remove trie a =
  let sz = Array.length a in
  QCheck.Test.make ~count:1000 ~name:"random remove" QCheck.(int_bound (sz - 1))
  @@ fun i ->
  let trie = T.remove a.(i) trie in
  not @@ T.mem a.(i) trie

(* TODO: mark this test as slow test. *)
let test_random_fuzzy_mem trie a =
  let sz = Array.length a in
  QCheck.Test.make ~count:3 ~name:"random fuzzy mem with dist <= 1"
    QCheck.(int_bound (sz - 1))
  @@ fun i ->
  let w1 = a.(i) in
  let expected = Array.exists (fun w2 -> distance w1 w2 <= 1) a in
  let result = T.fuzzy_mem ~max_dist:1 w1 trie in
  expected = result

let create_index path =
  Compat.In_channel.with_open_text path @@ fun ic ->
  let rec loop t l i =
    match In_channel.input_line ic with
    | None -> (t, Array.of_list l)
    | Some line -> loop (T.add line i t) (line :: l) (i + 1)
  in
  loop T.empty [] 1

let () =
  match Array.to_list Sys.argv with
  | x :: dict :: xs ->
      let argv = Array.of_list (x :: xs) in
      let trie, a = create_index dict in
      let quick_test s tst = A.test_case s `Quick (tst trie a) in
      let qcheck_test tst = QCheck_alcotest.to_alcotest (tst trie a) in
      A.run ~argv __FILE__
        [
          ( "index operations",
            [
              quick_test "cardinal" test_cardinal;
              quick_test "mem" test_search;
              quick_test "search" test_search;
              quick_test "remove" test_remove;
              quick_test "update" test_update;
              quick_test "lexicography_order" test_lexicographic_order;
            ] );
          ( "random index operations",
            [
              qcheck_test test_random_mem;
              qcheck_test test_random_search;
              qcheck_test test_random_remove;
              qcheck_test test_random_fuzzy_mem;
            ] );
        ]
  | _ -> failwith "expected a dictionary file in txt format as first argument"
