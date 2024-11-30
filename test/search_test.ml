module A = Alcotest
module I = Geneweb_search.Index.Default

let test_cardinal idx _a () =
  let idx = I.of_list [ ("foo", ()); ("bar", ()); ("saucisse", ()) ] in
  A.(check int) "cardinal after insertion" 3 (I.cardinal idx)

let test_mem idx _a () =
  A.(check bool) "mem Vieux-Fort" true (I.mem "Vieux-Fort" idx);
  A.(check bool) "mem Vieu" false (I.mem "Vieu" idx);
  A.(check bool) "mem Pariso" false (I.mem "Pariso" idx)

let test_search idx _a () =
  A.(check int) "search Vieux-Fort" 1 (Seq.length @@ I.search "Vieux-Fort" idx)

let test_remove idx _a () =
  A.(check bool) "remove Paris (before)" true (I.mem "Paris" idx);
  let idx = I.remove "Paris" idx in
  A.(check bool) "remove Paris (after)" false (I.mem "Paris" idx)

let test_lexicographic_order _idx _a () =
  let idx =
    I.of_list [ ("abe", ()); ("ab", ()); ("a", ()); ("bcd", ()); ("abcd", ()) ]
  in
  let expected = [ ("a", ()); ("ab", ()); ("abcd", ()); ("abe", ()) ] in
  A.(check (list (pair string unit)))
    "order" expected
    (List.of_seq @@ I.search "a" idx)

let test_random_mem idx a =
  let sz = Array.length a in
  QCheck.Test.make ~count:1000 ~name:"random mem" QCheck.(int_bound (sz - 1))
  @@ fun i -> I.mem a.(i) idx

let test_random_search idx a =
  let sz = Array.length a in
  QCheck.Test.make ~count:1000 ~name:"random search" QCheck.(int_bound (sz - 1))
  @@ fun i -> not @@ Seq.is_empty @@ I.search a.(i) idx

let test_random_remove idx a =
  let sz = Array.length a in
  QCheck.Test.make ~count:1000 ~name:"random remove" QCheck.(int_bound (sz - 1))
  @@ fun i ->
  let idx = I.remove a.(i) idx in
  not @@ I.mem a.(i) idx

let create_index path =
  File.with_in_channel path @@ fun ic ->
  let rec loop t l i =
    match In_channel.input_line ic with
    | None -> (t, Array.of_list l)
    | Some line -> loop (I.insert line i t) (line :: l) (i + 1)
  in
  loop I.empty [] 1

let () =
  match Array.to_list Sys.argv with
  | x :: dict :: xs ->
      let argv = Array.of_list (x :: xs) in
      let idx, a = create_index dict in
      let quick_test s tst = A.test_case s `Quick (tst idx a) in
      let qcheck_test tst = QCheck_alcotest.to_alcotest (tst idx a) in
      A.run ~argv __FILE__
        [
          ( "index operations",
            [
              quick_test "cardinal" test_cardinal;
              quick_test "mem" test_search;
              quick_test "search" test_search;
              quick_test "remove" test_remove;
              quick_test "lexicography_order" test_lexicographic_order;
            ] );
          ( "random index operations",
            [
              qcheck_test test_random_mem;
              qcheck_test test_random_search;
              qcheck_test test_random_remove;
            ] );
        ]
  | _ -> failwith "expected a dictionary file in txt format as first argument"
