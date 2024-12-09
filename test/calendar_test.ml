let data_sure =
  [
    Def.{ day = 1; month = 1; year = 1900; delta = 0; prec = Sure };
    Def.{ day = 0; month = 1; year = 1900; delta = 0; prec = Sure };
    Def.{ day = 0; month = 0; year = 1900; delta = 0; prec = Sure };
  ]

let data_oryear =
  [
    Def.
      {
        day = 1;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 1; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Def.
      {
        day = 0;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 0; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Def.
      {
        day = 0;
        month = 0;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 0; month2 = 0; year2 = 1901; delta2 = 0 };
      };
  ]

open Alcotest
open Calendar

let testable_calendar = testable Adef.pp_dmy ( = )

exception Fail_expected

let round_trip ?(fail_expected = false) of_ to_ l () =
  let f d = of_ (to_ d) in
  (* todo should iter in v? *)
  match List.iter (fun d -> (check testable_calendar) "" d (f d)) l with
  | exception _ when fail_expected -> ()
  | () when fail_expected -> raise Fail_expected
  | () -> ()

let () =
  Alcotest.run __FILE__
    [
      ( (* this fail because Calendars library does not work on incomplete dates (day|month) = 0 *)
        "calendar-sdn",
        [
          test_case "Calendar gregorian <-> sdn" `Quick
            (round_trip ~fail_expected:true (gregorian_of_sdn Def.Sure) sdn_of_gregorian data_sure);
          test_case "Calendar julian <-> sdn" `Quick
            (round_trip ~fail_expected:true (julian_of_sdn Def.Sure)
               sdn_of_julian data_sure);
          test_case "Calendar french <-> sdn" `Quick
            (round_trip ~fail_expected:true (french_of_sdn Def.Sure)
               sdn_of_french data_sure);
          test_case "Calendar hebrew <-> sdn" `Quick
            (round_trip ~fail_expected:true (hebrew_of_sdn Def.Sure)
               sdn_of_hebrew data_sure);
        ] );
      ( "calendar-greg",
        [
          test_case "Calendar gregorian <-> julian" `Quick
            (round_trip gregorian_of_julian julian_of_gregorian
               (data_sure @ data_oryear));
          test_case "Calendar gregorian <-> french" `Quick
            (round_trip gregorian_of_french french_of_gregorian
               (data_sure @ data_oryear));
          test_case "Calendar gregorian <-> hebrew" `Quick
            (round_trip gregorian_of_hebrew hebrew_of_gregorian
               (data_sure @ data_oryear));
        ] );
    ]

(*

let suite =
  [
    "Calendar"
    >::: []
         (* @ (sdn_round_trip "gregorian" Calendar.sdn_of_gregorian Calendar.gregorian_of_sdn)
          * @ (sdn_round_trip "julian" Calendar.sdn_of_julian Calendar.julian_of_sdn)
          * @ (sdn_round_trip "french" Calendar.sdn_of_french Calendar.french_of_sdn)
          * @ (sdn_round_trip "hebrew" Calendar.sdn_of_hebrew Calendar.hebrew_of_sdn) *)
  ]
  *)
