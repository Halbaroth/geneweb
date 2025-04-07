(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open MergeInd

let print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 (title : Geneweb_sanatize.Sanatize.safe_string)
      (name : Geneweb_sanatize.Sanatize.encoded_string) proj =
    let name = (name :> string) in
    let x1 : Geneweb_sanatize.Sanatize.safe_string = proj p1 in
    let x2 : Geneweb_sanatize.Sanatize.safe_string = proj p2 in
    if
      (x1 :> string) <> ""
      && (x1 :> string) <> "?"
      && (x2 :> string) <> ""
      && (x2 :> string) <> "?"
      && x1 <> x2
    then (
      let aux i x chk =
        Output.print_sstring conf
          {|<div class="custom-control custom-radio ml-3">|};
        Output.print_sstring conf
          {|<input class="custom-control-input" type="radio" id="|};
        Output.print_sstring conf name;
        Output.print_sstring conf (string_of_int i);
        Output.print_sstring conf {|" name="|};
        Output.print_sstring conf name;
        Output.print_sstring conf {|" value="|};
        Output.print_sstring conf (string_of_int i);
        Output.print_sstring conf {|"|};
        if chk then Output.print_sstring conf " checked";
        Output.print_sstring conf {|>|};
        Output.printf conf {|<label class="custom-control-label" for="|};
        Output.print_sstring conf name;
        Output.print_sstring conf (string_of_int i);
        Output.print_sstring conf {|">|};
        Output.print_string conf x;
        Output.print_sstring conf {|</label></div>|}
      in
      Output.print_sstring conf "<h4>";
      Output.print_string conf
        (Geneweb_sanatize.Sanatize.safe_fn Utf8.capitalize_fst title);
      Output.print_sstring conf {|</h4>|};
      aux 1 x1 chk1;
      aux 2 x2 chk2)
  in
  let string_field = gen_string_field true false in
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  Output.print_sstring conf "<p>\n";
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Geneweb_sanatize.Sanatize.encoded "MRG_IND_OK");
  Util.hidden_input conf "i1" (get_iper p1 |> string_of_iper |> Mutil.encode);
  Util.hidden_input conf "i2" (get_iper p2 |> string_of_iper |> Mutil.encode);
  let rec loop = function
    | [ (ip1, ip2) ] ->
        Util.hidden_input conf "ini1" (ip1 |> string_of_iper |> Mutil.encode);
        Util.hidden_input conf "ini2" (ip2 |> string_of_iper |> Mutil.encode)
    | _ :: branches -> loop branches
    | [] -> ()
  in
  loop branches;
  (match (p_getenv conf.env "m", p_getint conf.env "ip") with
  | Some "MRG_DUP_IND_Y_N", Some ip ->
      Util.hidden_input conf "ip"
        (ip |> string_of_int |> Geneweb_sanatize.Sanatize.encoded);
      List.iter
        (fun excl ->
          match p_getenv conf.env excl with
          | None | Some "" -> ()
          | Some s -> Util.hidden_input conf excl (Mutil.encode s))
        [ "iexcl"; "fexcl" ]
  | _ -> ());
  Output.print_sstring conf "</p><p>";
  string_field
    (transl_nth conf "first name/first names" 0
    |> Geneweb_sanatize.Sanatize.safe)
    ("first_name" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p ->
      (p_first_name base p |> escape_html
        :> Geneweb_sanatize.Sanatize.safe_string));
  string_field
    (transl_nth conf "surname/surnames" 0 |> Geneweb_sanatize.Sanatize.safe)
    ("surname" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p ->
      (p_surname base p |> escape_html :> Geneweb_sanatize.Sanatize.safe_string));
  let select_smallest_num = p_first_name base p1 = p_first_name base p2 in
  gen_string_field
    (get_occ p1 < get_occ p2 || not select_smallest_num)
    (get_occ p1 > get_occ p2 && select_smallest_num)
    (transl conf "number" |> Geneweb_sanatize.Sanatize.safe)
    ("number" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p -> get_occ p |> string_of_int |> Geneweb_sanatize.Sanatize.safe);
  string_field
    (transl_nth conf "image/images" 0 |> Geneweb_sanatize.Sanatize.safe)
    ("image" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p ->
      match Image.get_portrait conf base p with
      | Some (`Url url) ->
          ({|<img src="|} ^<^ escape_html url
           ^>^ {|" style="max-width:75px;max-height:100px">|} ^ url
            :> Geneweb_sanatize.Sanatize.safe_string)
      | Some (`Path path) ->
          let k = Image.default_portrait_filename base p in
          let s = Unix.stat path in
          Printf.sprintf
            {|<img src="%sm=IM&d=%s&%s&k=%s" \
              style="max-width:75px;max-height:100px"> %s|}
            (commd conf :> string)
            (string_of_int
            @@ int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
            (acces conf base p
              : Geneweb_sanatize.Sanatize.escaped_string
              :> string)
            k path
          |> Geneweb_sanatize.Sanatize.safe
      | None -> Geneweb_sanatize.Sanatize.safe "");
  string_field
    (transl conf "public name" |> Geneweb_sanatize.Sanatize.safe)
    ("public_name" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p ->
      (get_public_name p |> sou base |> escape_html
        :> Geneweb_sanatize.Sanatize.safe_string));
  string_field
    (transl_nth conf "occupation/occupations" 0
    |> Geneweb_sanatize.Sanatize.safe)
    ("occupation" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p ->
      (get_occupation p |> sou base |> escape_html
        :> Geneweb_sanatize.Sanatize.safe_string));
  string_field
    (transl conf "sex" |> Geneweb_sanatize.Sanatize.safe)
    ("sex" |> Geneweb_sanatize.Sanatize.encoded)
    (fun p ->
      Geneweb_sanatize.Sanatize.safe
      @@ match get_sex p with Male -> "M" | Female -> "F" | Neuter -> "");
  let date_field trans name get =
    string_field
      (transl conf trans |> Geneweb_sanatize.Sanatize.safe)
      name
      (fun p ->
        match Date.od_of_cdate (get p) with
        | None -> Geneweb_sanatize.Sanatize.safe ""
        | Some d -> DateDisplay.string_of_ondate conf d)
  in
  let place_field trans name get =
    string_field
      (transl conf trans
      ^<^ Geneweb_sanatize.Sanatize.safe " / "
      ^>^ transl_nth conf "place/places" 0)
      name
      (fun p -> get p |> sou base |> safe_html)
  in
  date_field "birth" (Geneweb_sanatize.Sanatize.encoded "birth") get_birth;
  place_field "birth"
    (Geneweb_sanatize.Sanatize.encoded "birth_place")
    get_birth_place;
  date_field "baptism" (Geneweb_sanatize.Sanatize.encoded "baptism") get_baptism;
  place_field "baptism"
    (Geneweb_sanatize.Sanatize.encoded "baptism_place")
    get_baptism_place;
  string_field
    (transl conf "death" |> Geneweb_sanatize.Sanatize.safe)
    (Geneweb_sanatize.Sanatize.encoded "death")
    (fun p ->
      let is = 2 in
      match get_death p with
      | NotDead -> transl_nth conf "alive" is |> Geneweb_sanatize.Sanatize.safe
      | Death (dr, cd) ->
          let s =
            match dr with
            | Killed -> transl_nth conf "killed (in action)" is
            | Murdered -> transl_nth conf "murdered" is
            | Executed -> transl_nth conf "executed (legally killed)" is
            | Disappeared -> transl_nth conf "disappeared" is
            | Unspecified -> transl_nth conf "died" is
          in
          s ^<^ " "
          ^<^ DateDisplay.string_of_ondate conf (Date.date_of_cdate cd)
      | DeadYoung ->
          transl_nth conf "died young" is |> Geneweb_sanatize.Sanatize.safe
      | DeadDontKnowWhen ->
          transl_nth conf "died" is |> Geneweb_sanatize.Sanatize.safe
      | DontKnowIfDead | OfCourseDead -> Geneweb_sanatize.Sanatize.safe "");
  place_field "death"
    (Geneweb_sanatize.Sanatize.encoded "death_place")
    get_death_place;
  string_field
    (transl conf "burial" |> Geneweb_sanatize.Sanatize.safe)
    (Geneweb_sanatize.Sanatize.encoded "burial")
    (fun p ->
      let is = 2 in
      (* TODO burial_to_string *)
      match get_burial p with
      | UnknownBurial -> Geneweb_sanatize.Sanatize.safe ""
      | Buried cod -> (
          transl_nth conf "buried" is
          ^<^
          match Date.od_of_cdate cod with
          | None -> Geneweb_sanatize.Sanatize.safe ""
          | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d)
      | Cremated cod -> (
          transl_nth conf "cremated" is
          ^<^
          match Date.od_of_cdate cod with
          | None -> Geneweb_sanatize.Sanatize.safe ""
          | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d));
  place_field "burial"
    (Geneweb_sanatize.Sanatize.encoded "burial_place")
    get_burial_place;
  Output.print_sstring conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_sstring conf {|</button></form>|}

let propose_merge_ind conf base branches p1 p2 =
  let title _ =
    let s = transl_nth conf "person/persons" 1 in
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  if branches <> [] then (
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl conf "you must first merge"));
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf "<ul><li><a href=\"";
    Output.print_string conf (commd conf);
    Output.print_string conf (acces conf base p1);
    Output.print_sstring conf "\">";
    MergeDisplay.print_someone conf base p1;
    Output.print_sstring conf "</a> ";
    Output.print_sstring conf (transl_nth conf "and" 0);
    Output.print_sstring conf " <a href=\"";
    Output.print_string conf (commd conf);
    Output.print_string conf (acces conf base p2);
    Output.print_sstring conf "\">";
    MergeDisplay.print_someone conf base p2;
    Output.print_sstring conf "</a></li></ul><p></p>");
  print_differences conf base branches p1 p2;
  if branches <> [] then (
    Output.print_sstring conf "<p><hr></p><p>";
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "branch/branches" 1));
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf "</p><table>";
    List.iter
      (fun (ip1, ip2) ->
        let p1 = poi base ip1 in
        let p2 = poi base ip2 in
        Output.print_sstring conf "<tr align=\"";
        Output.print_sstring conf conf.left;
        Output.print_sstring conf "\"><td>";
        Output.print_string conf (referenced_person_text conf base p1);
        Output.print_string conf (DateDisplay.short_dates_text conf base p1);
        Output.print_sstring conf "</td><td>";
        Output.print_string conf (referenced_person_text conf base p2);
        Output.print_string conf (DateDisplay.short_dates_text conf base p2);
        Output.print_sstring conf "</td></tr>")
      ((get_iper p1, get_iper p2) :: branches);
    Output.print_sstring conf "</table>");
  Hutil.trailer conf

let error_loop conf base p =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.print_sstring conf "<strong>";
  Output.print_string conf (p_first_name base p |> escape_html);
  if get_occ p <> 0 then (
    Output.print_sstring conf ".";
    Output.print_sstring conf (get_occ p |> string_of_int));
  Output.print_sstring conf " ";
  Output.print_string conf (p_surname base p |> escape_html);
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "would be his/her own ancestor");
  Output.print_sstring conf "";
  Hutil.trailer conf

let propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title _ =
    transl_nth conf "family/families" 1
    |> transl_decline conf "merge"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  transl conf "you must first merge the 2 families"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf "<ul><li><a href=\"";
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p1);
  Output.print_sstring conf "\">";
  MergeDisplay.print_someone conf base p1;
  Output.print_sstring conf "</a> ";
  Output.print_sstring conf (transl conf "with");
  Output.print_sstring conf " <a href=\"";
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p2);
  Output.print_sstring conf "\">";
  MergeDisplay.print_someone conf base p2;
  Output.print_sstring conf "</a></li></ul><p>";
  MergeFamDisplay.print_differences conf base branches fam1 fam2;
  Hutil.trailer conf

let not_found_or_incorrect conf =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "not found"));
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "or");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "several answers");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "or");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "incorrect request");
  Hutil.trailer conf

let same_person conf =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "error"))
  in
  Hutil.rheader conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl conf "it is the same person!"));
  Hutil.trailer conf

let different_sexes conf base p1 p2 =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "error"))
  in
  Hutil.rheader conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl conf "incompatible sexes"));
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf {|<ul><li><a href="|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p1);
  Output.print_sstring conf {|">|};
  Output.print_string conf (Util.designation base p1);
  Output.print_sstring conf {|</a></li><li>|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p2);
  Output.print_sstring conf {|">|};
  Output.print_string conf (Util.designation base p2);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let print_merged conf base wl p =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "merge done"))
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  (match (p_getenv conf.env "m", p_getenv conf.env "ip") with
  | Some "MRG_DUP_IND_Y_N", Some ip ->
      let ip = iper_of_string ip in
      let s1 =
        match p_getenv conf.env "iexcl" with
        | Some "" | None -> Geneweb_sanatize.Sanatize.encoded ""
        | Some s -> "&iexcl=" ^<^ Mutil.encode s
      in
      let s2 =
        match p_getenv conf.env "fexcl" with
        | Some "" | None -> Geneweb_sanatize.Sanatize.encoded ""
        | Some s -> "&fexcl=" ^<^ Mutil.encode s
      in
      Output.print_sstring conf "<p>";
      Output.print_sstring conf "<a href=";
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=MRG_DUP&ip=";
      Output.print_string conf (string_of_iper ip |> Mutil.encode);
      Output.print_string conf s1;
      Output.print_string conf s2;
      Output.print_sstring conf ">";
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "continue merging"));
      Output.print_sstring conf "</a>";
      (let p = poi base ip in
       let s = gen_person_text conf base p in
       Output.print_sstring conf "\n(";
       Output.print_sstring conf
         (Util.transl_a_of_b conf
            (transl conf "possible duplications")
            (reference conf base p s :> string)
            (s :> string));
       Output.print_sstring conf ")\n");
      Output.print_sstring conf "</p>\n"
  | _ -> ());
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print conf base =
  let p1 =
    match p_getenv conf.env "i" with
    | Some i1 -> Some (poi base (iper_of_string i1))
    | None -> None
  in
  let p2 =
    match p_getenv conf.env "i2" with
    | Some i2 -> Some (poi base (iper_of_string i2))
    | None -> (
        match (p_getenv conf.env "select", p_getenv conf.env "n") with
        | (Some "input" | None), Some n -> (
            let ipl = Gutil.person_ht_find_all base n in
            match ipl with [ ip2 ] -> Some (poi base ip2) | _ -> None)
        | Some x, (Some "" | None) -> Some (poi base (iper_of_string x))
        | _ -> None)
  in
  match (p1, p2) with
  | Some p1, Some p2 -> (
      try
        let ok, warnings =
          merge conf base p1 p2 propose_merge_ind propose_merge_fam
        in
        if ok then print_merged conf base warnings p1
      with
      | Error_loop p -> error_loop conf base p
      | Different_sexes (p1, p2) -> different_sexes conf base p1 p2
      | Same_person -> same_person conf)
  | _ -> not_found_or_incorrect conf

(* Undocumented feature... Kill someone's ancestors *)

let print_killed conf base p nb_ind nb_fam =
  let title _ = Output.print_sstring conf "Ancestors killed" in
  Hutil.header conf title;
  Output.print_string conf (referenced_person_title_text conf base p);
  Output.print_sstring conf "'s ancestors killed.<br>";
  Output.print_sstring conf (string_of_int nb_ind);
  Output.print_sstring conf " persons and ";
  Output.print_sstring conf (string_of_int nb_fam);
  Output.print_sstring conf " families deleted<p>";
  Hutil.trailer conf

let print_kill_ancestors conf base =
  match List.assoc_opt "can_kill_ancestors" conf.base_env with
  | Some "yes" -> (
      match find_person_in_env conf base "" with
      | Some p ->
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          kill_ancestors conf base false p nb_ind nb_fam;
          Util.commit_patches conf base;
          let changed =
            U_Kill_ancestors
              (Util.string_gen_person base (gen_person_of_person p))
          in
          History.record conf base changed "ka";
          print_killed conf base p !nb_ind !nb_fam
      | None -> Hutil.incorrect_request conf)
  | _ -> Hutil.incorrect_request conf
