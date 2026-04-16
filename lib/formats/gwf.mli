type 'a field
type path = string
type passwd = private { login : string option; passwd : string }

module Fields : sig
  val access_by_key : bool field
  val show_consang : bool field
  val public_if_titles : bool field
  val public_if_no_date : bool field
  val display_sosa : bool field
  val full_sibling : bool field
  val default_image : bool field
  val visitor_access : bool field
  val no_image_for_visitor : bool field
  val no_note_for_visitor : bool field
  val friend_passwd : passwd option field
  val friend_passwd_file : path option field
  val wizard_passwd : passwd option field
  val wizard_just_friend : bool field
  val authorized_wizards_notes : bool field
  val hide_private_names : bool field
  val use_restrict : bool field
  val can_send_image : bool field
  val propose_titles : bool field
  val propose_add_family : bool field
  val propose_place_surnames : bool field
  val hide_advanced_request : bool field
  val counter : bool field
  val places_inverted : bool field
  val place_surname_link_to_ind : bool field
  val max_rlm_nbr : int field
  val max_anc_level : int field
  val max_anc_tree : int option field
  val max_desc_level : int field
  val max_desc_tree : int field
  val max_cousins : int field
  val max_cousins_level : int field
  val cache_cousins_tool : bool field
  val cache_cousins_ttl : int field
  val latest_event : int field
  val always_surname : bool field
  val history : bool field
  val history_diff : bool field
  val history_path : string field
  val datalist_fnames : int field
  val datalist_places : int field
  val datalist_occupations : int field
  val datalist_sources : int field
  val datalist_pub_names : int field
  val datalist_qualifiers : int field
  val datalist_aliases : int field
  val datalist_titles : int field
  val datalist_estates : int field
  val rpc_datalist : int field
  val rpc_server_url : string field
  val renamed : string option field
  val moved : string option field
  val gzip_html_compression : int field
  val template : string list field
  val expand_env : bool field
  val perso_module_a : string option field
  val perso_module_b : string option field
  val perso_module_c : string option field
  val perso_module_d : string option field
  val perso_module_e : string option field
  val perso_module_f : string option field
  val perso_module_g : string option field
  val perso_module_h : string option field
  val perso_module_i : string option field
  val perso_module_j : string option field
  val perso_module_k : string option field
  val perso_module_l : string option field
  val perso_module_m : string option field
  val perso_module_n : string option field
  val perso_module_o : string option field
  val perso_module_p : string option field
  val perso_module_q : string option field
  val perso_module_r : string option field
  val perso_module_s : string option field
  val perso_module_t : string option field
  val perso_module_u : string option field
  val perso_module_v : string option field
  val perso_module_w : string option field
  val perso_module_x : string option field
  val perso_module_y : string option field
  val perso_module_z : string option field
  val occu_in_homonyms : int field
  val p_mod : string option field
  val book_max_results : int field
  val chk_data_max_results : int field
  val max_nb_update : int field
  val use_cdn : bool field
  val hide_querytime_bugs : bool field
  val origin_file : bool field
  val can_kill_ancestors : bool field
  val disable_forum : bool field
  val hide_connection : bool field
  val manitou : string option field
  val moderator_file : string option field
  val supervisor : string option field
  val forum_exclude_file : string option field
  val welcome_logo : string option field
  val welcome_logo_style : string option field
  val css_prop : string option field
  val highlight_color : string option field
  val body_prop : string option field
  val short_place_threshold : int field
  val max_ancestor_implex : int field
  val long_date : bool field
  val fast_alphabetic : bool field
  val allowed_titles_file : string option field
  val denied_titles_file : string option field
  val print_advanced_title : bool field
  val notify_change : string option field
  val vowels : string field
  val propose_alias : bool field
  val propose_qualifier : bool field
  val default_image_no : string option field
  val dump_bad_images : bool field
  val has_events : bool field
  val event_tooltips : bool field
  val notes_alias_file : string option field
  val robot_index : bool field
  val robot_index_forum : bool field
  val plugins : string list field
  val roglo : bool field
  val alt_serv_adr : string option field
  val alt_serv_base : string option field
  val alt_serv_title : string option field
  val red_if_not_exist : bool field
  val ptempl : bool field
  val multi_parents : bool field
  val css : int field
  val datalist_fn : int option field
  val datalist_sn : int option field
  val datalist_occu : int option field
  val datalist_place : int option field
  val datalist_src : int option field
  val datalist_book : int option field
  val event_age : bool field
  val jquery : bool field
  val maxlev : int field
  val max_dates : int field
  val menu : int field
  val misc : bool field
  val move_comment : bool field
  val particles : string option field
  val setup_link : bool field
  val setup_http : string option field
  val show_flag : bool field
  val timeline : bool field
  val uppercase : bool field
  val wide : bool field
end

module Format : sig
  type t

  val v1 : t
  val latest : t
  val pp : t Fmt.t
end

type format = Format.t
type t

val read : in_channel -> format -> t
val field : 'a field -> t -> 'a
