module Types = Geneweb_types

type passwd = { login : string option; passwd : string }

let passwd =
  let compare { login = l1; passwd = p1 } { login = l2; passwd = p2 } =
    let c = Option.compare String.compare l1 l2 in
    if c <> 0 then c else String.compare p1 p2
  in
  let of_json _ = assert false in
  let to_json _ = assert false in
  let pp ppf { login; passwd } =
    Fmt.pf ppf "%a:%s" Fmt.(option ~none:nop string) login passwd
  in
  Types.make ~name:"passwd" ~compare ~of_json ~to_json ~pp ()

type path = string

let path = Types.Syntax.string

type 'a field = {
  name : string;
  default : 'a;
  deprecated : bool;
  doc : string option;
  wit : 'a Types.t;
}

let extract_name s =
  match String.rindex s '.' with
  | exception Not_found -> s
  | i -> String.sub s (i + 1) (String.length s - i - 1)

let[@inline] mk ?(deprecated = false) ?doc ~default name wit =
  let name = extract_name name in
  { name; default; deprecated; doc; wit }

let compare { name = n1; _ } { name = n2; _ } = String.compare n1 n2

let pp_doc ppf doc =
  let lines = String.split_on_char '\n' doc in
  let pp_comment ppf s = Fmt.pf ppf "# %s" s in
  Fmt.(list ~sep:Format.pp_force_newline pp_comment) ppf lines

let pp_field ppf { name; default; doc; wit; deprecated } =
  Option.iter (fun d -> Fmt.pf ppf "%a@\n" pp_doc d) doc;
  if deprecated then Fmt.pf ppf "#THIS OPTION IS DEPRECATED@\n";
  Fmt.pf ppf "#%s=%a@\n" name (Types.pp_value wit) default

module Fields = struct
  module S = Types.Syntax

  let access_by_key =
    let doc =
      "Access by key, when set to \"yes\", generates HTML requests with \n\
       \"p=first+name;n=surname;oc=number\" instead of (shorter) \n\
       \"i=identifier\" in URLs which is default."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let show_consang =
    let doc =
      "Show consanguinity in personal pages if computed (default) set to no \n\
       if needed."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let public_if_titles =
    let doc =
      "Individuals with titles are public if set (visitors can see them)."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let public_if_no_date =
    let doc =
      "Individuals without dates are public if set, except if their access \n\
       is explicitly set to “private”."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let display_sosa =
    let doc =
      "Display the picto Sosa on the template perso.txt (individual page). \n\
       assuming default_sosa_ref is defined."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let full_sibling =
    let doc =
      "Display the individual in his ’own’ siblings on the template \n\
       perso.txt (individual page), except if set to 'no'."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let default_image =
    let doc =
      "Display a default man/woman/neuter image when individual image is \n\
       missing (not implemented on all pages)."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let visitor_access =
    let doc = "Restrict access to friends and wizards." in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let no_image_for_visitor =
    let doc =
      "Don’t search images if for visitor (can be useful if the images are \n\
       hosted in a site protected by a password that only wizards and friend \n\
       know."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let no_note_for_visitor =
    let doc =
      "Don’t search notes if normal visitor (i.e. neither \"wizard\" nor \n\
       \"friend\"). Notes = individual note or Marriage note."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let friend_passwd =
    let doc =
      "Friends access password. allows global access control for persons \n\
       potentially still alive. If a value is supplied for this variable, only \n\
       visitors having entered those as friend_id/password will see data about \n\
       persons born less than “private_years” years. Form: \n\
       \"friend_id:password\" or just \"password\""
    in
    mk ~default:None ~doc __FUNCTION__ S.(option passwd)

  let friend_passwd_file =
    let doc =
      "Alternative way for friend entry: authorization file. The file must \n\
       hold lines of the form “user:password”."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option path)

  let wizard_passwd =
    let doc =
      "Wizards access password. Form: \"wizard_id:password\" or just \n\
       \"password\"."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option passwd)

  let wizard_just_friend =
    let doc = "Remove all wizards’ powers. Wizards become friends." in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let authorized_wizards_notes =
    let doc =
      "Authorized wizards notes. By default, wizards notes are not allowed."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let hide_private_names =
    let doc =
      "If set, the names of the private persons (less than private_years old) \n\
       are not displayed: \"x x\" is displayed instead. To see the real names, \n\
       one must be \"wizard\" or \"friend\"."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let use_restrict =
    let doc =
      "More restricted system than hide_private_names. if set the hidden \n\
       persons are not clickable, and the links to their children, ancestors \n\
       and spouses are not accessible."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let can_send_image =
    let doc = "Don’t authorise wizards to send images." in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let propose_titles =
    let doc = "Hide “search by titles” be on the welcome page." in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let propose_add_family =
    let doc =
      "Hide wizards \"add family\" button on welcome page (forbid also adding \n\
       families not connected to the rest on the database)."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let propose_place_surnames =
    let doc =
      "Hide “Places/surname” button on welcome page. By default button is \n\
       accessible."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let hide_advanced_request =
    let doc =
      "Hide “advanced request” button on welcome page. Hiden by default since \n\
       gw version 4.0."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let counter =
    let doc = "Hide “counter of visits” on welcome page." in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let places_inverted =
    let doc =
      "Tell that in this database the order of places are registered inverted \n\
       (being more general to less general), e.g. \"USA, New York\" instead of \n\
       \"New-York, USA\". Set it to \"yes\" if it is the case. Important for \n\
       display by Places/surname."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let place_surname_link_to_ind =
    let doc =
      "In the Places/surname page, the surname string may be a link to list \n\
       all individuals of that surname, not only those in related place."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let max_rlm_nbr =
    let doc =
      "The max number for relation list is the list size limit for \"list of \n\
       individuals by Place\" each list accessible from \"Places/surname\" \n\
       page. A list is not generated if number of individuals is greater."
    in
    mk ~default:80 ~doc __FUNCTION__ S.int

  let max_anc_level =
    let doc = "Maximum number of generations when displaying ancestors." in
    mk ~default:13 ~doc __FUNCTION__ S.int

  let max_anc_tree =
    let doc =
      "Maximum number of generations when displaying ancestors by tree if not \
       specified then limited by max_anc_level."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option int)

  let max_desc_level =
    let doc = "Maximum number of generations when displaying descendants." in
    mk ~default:12 ~doc __FUNCTION__ S.int

  let max_desc_tree =
    let doc =
      "Maximum number of generations when displaying descendants by tree."
    in
    mk ~default:6 ~doc __FUNCTION__ S.int

  let max_cousins =
    let doc = "Maximum number of displayed cousins." in
    mk ~default:2_000 ~doc __FUNCTION__ S.int

  let max_cousins_level =
    let doc = "Maximum level of displayed cousins." in
    mk ~default:6 ~doc __FUNCTION__ S.int

  let cache_cousins_tool =
    let doc =
      "Use cache file for cousins tool (m=C). disabled by default Usefull when \
       self has a lot of relationships."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let cache_cousins_ttl =
    let doc =
      "Cache time-to-live in hours (files older than this are auto-deleted) \
       Default: 1 hour. Set to 0 to disable auto-cleanup (permanent cache)."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let latest_event =
    let doc =
      "Number of latest events (birth, death) displayed in statistics."
    in
    mk ~default:20 ~doc __FUNCTION__ S.int

  let always_surname =
    let doc =
      "Always display the children surnames even if same than father. Applies \n\
       in descendants page, in surname displaying page, also in siblings and \n\
       union modules. default is to not display if same surname. Note for \n\
       templm: if variable is set in gwf, then user may force the reverse by \n\
       manually adding always_surname=no in query url."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let history =
    let doc =
      "Allow an historic file that log all updates in database in a file named \n\
       “history” in the database directory. This file grows indefinitively."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let history_diff =
    let doc =
      "Allow to see differences between modifications from history. Diff files \n\
       are stored in a database subdirectory or the specified “history_path” \n\
       directory (see below) that grows indefinitively."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let history_path =
    let doc = "Path to store all the histories (default \"history_d\")." in
    mk ~default:"history_d" ~doc __FUNCTION__ path

  let datalist_fnames =
    let doc =
      "First names and surnames Used in: welcome page, individual and family \n\
       modification forms."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_places =
    let doc = "Places used in: individual and family modification forms." in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_occupations =
    let doc =
      "Occupations used in: individual and family modification forms."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_sources =
    let doc = "Sources used in: individual and family modification forms." in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_pub_names =
    let doc = "Public names used in: individual modification form." in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_qualifiers =
    let doc = "Qualifiers used in: individual modification form." in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_aliases =
    let doc = "Aliases used in: individual modification form." in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_titles =
    let doc =
      "Nobility titles used in: welcome page and individual modification forms."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_estates =
    let doc =
      "Domains used in: welcome page and individual modification forms."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let rpc_datalist =
    let doc =
      "Enable RPC datalists (for non-Roglo bases). Roglo bases use RPC by \n\
       default."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let rpc_server_url =
    let doc =
      "RPC server address (without port). Default: localhost The server \n\
       listens on port 8080 with endpoint /search Examples: localhost, \n\
       192.168.1.100, rpc.myserver.org."
    in
    mk ~default:"localhost" ~doc __FUNCTION__ S.string

  let renamed =
    let doc =
      "Indicate that the present database has been renamed to “newname”."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let moved =
    let doc =
      "Indicate that the database has been moved to “new.adress” website.\n\
      \  https://new.address:2317/..."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let gzip_html_compression =
    let doc =
      "Gzip compression for HTML responses Controls gzip compression level for\n\
      \  dynamically generated HTML and JSON.\n\
      \  - 0: disabled (no compression, enables progressive HTML rendering)\n\
      \  - 1-9: compression level (1=fastest, 9=smallest, 6=default balance) \n\
       Static\n\
      \    assets (JS/CSS) use pre-compressed .gz files when available."
    in
    mk ~default:6 ~doc __FUNCTION__ S.int

  let template =
    let doc =
      "Default is template=database-name,* The template to be used can be \n\
       changed by adding \";templ=foo\" in the URL."
    in
    mk ~default:[] ~doc __FUNCTION__ S.(list string)

  let expand_env =
    let doc =
      "If yes, expand the (system) environment variables found in the values of\n\
      \      the customized variables. You can write ${xxx} to expand the \
       variable\n\
      \      xxx. E.g., if HOME is /home/smith and if you define:\n\
      \         var_foo=my home is ${HOME}, guys!\n\
      \      the customized variable %vfoo; is expanded into:\n\
      \         my home is /home/smith, guys!\n\
      \      It may be a security hole to allow accommodated databases in a \
       Web site to\n\
      \      show the environment variables of the command gwd."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let perso_module_a = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_b = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_c = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_d = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_e = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_f = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_g = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_h = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_i = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_j = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_k = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_l = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_m = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_n = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_o = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_p = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_q = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_r = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_s = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_t = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_u = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_v = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_w = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_x = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_y = mk ~default:None __FUNCTION__ S.(option string)
  let perso_module_z = mk ~default:None __FUNCTION__ S.(option string)

  let occu_in_homonyms =
    let doc =
      "Display occupation in homonyms lists (name conflict resolution page) \n\
       Useful for bases where occupation helps distinguish individuals (e.g., \n\
       \"Weaver Josef Kastner\" vs \"Shoemaker Josef Kastner\") 0 or absent: \n\
       disabled yes : enabled, no truncation n : truncate to n characters with \n\
       ellipsis."
    in
    mk ~default:5 ~doc __FUNCTION__ S.int

  let p_mod =
    let doc =
      "The display on perso pages will be governed by a vector defining a \n\
       selection of modules and the order in which they are displayed.\n\n\
      \  The vector appears as a string of letters and digits Each pair \
       \"letter, digit\" identifies a module amongst the list above, and the \
       value of its display option The modules are displayed in the order in \
       which they appear in \"p_mod\" In the example below, \"gr_parents\", \
       \"individu\", \"parents\", \"unions\"... p_mod=g1i2p1u3 By default when \
       not set (or 'zz'), the display is a static choice of module."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let book_max_results =
    let doc =
      "Limits the results of all books (m=MOD_DATA) accessible by wizard."
    in
    mk ~default:1_000 ~doc __FUNCTION__ S.int

  let chk_data_max_results =
    let doc =
      "Limits the results of data typographic checker (m=CHK_DATA) accessible \n\
       by wizard This variable allows to set up the maximum number of entries \n\
       with errors that can be displayed at a single time to avoid server \n\
       timeout on large databases. Default value if not specified is 500. If \n\
       specified as empty string, no limit will be applied. Examples: \n\
       chk_data_max_results=1500 # Limit to 1 500 entries \n\
       chk_data_max_results= # No limit (may cause timeout on large bases)."
    in
    mk ~default:500 ~doc __FUNCTION__ S.int

  let max_nb_update =
    let doc =
      "This variable allows to set up the maximum number of updates that can \n\
       be done at a single time. Default value if not specified is 5000 (which \n\
       means that it will take at most 5 s to realize a set of modifications \n\
       (such as the “set of places”, “set of sources”… If specified, the value \n\
       will be limited to 5000."
    in
    mk ~default:5_000 ~doc __FUNCTION__ S.int

  let use_cdn =
    let doc =
      "Use a distant CDN (Content Delivery Network) to load CSS/JS files \n\
       instead of loading them by the GWD server."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let hide_querytime_bugs =
    let doc =
      "Hide the hourglass and bug icons on top left corner that reports the \n\
       delta time of last query action and potiential errors."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let origin_file =
    let doc =
      "in case of multiple input gw files, the origin_file name is displayed \n\
       in individual page, in wizard mode, if evar opt=from. variable below \n\
       allow to update origin_file name in family update page. (default is to \n\
       not update)."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let can_kill_ancestors =
    let doc =
      "Allow the usage of the request m=KILL_ANC (templm) This was a [WIP] \n\
       since version 3.11 because no defined button, links intermediate pages \n\
       to ask for confirmation, things like that."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let disable_forum =
    let doc =
      "Disable the forum (all the request on the forum are “incorect request”."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let hide_connection =
    let doc =
      "Hide number of connected users/friends/wizards in footer (copyr \n\
       template)."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let manitou =
    let doc =
      "Manitou is a wizard who can: - delete any forum message - edit any \n\
       wizard’s notes - see all connected wizards - always see consanguinities \n\
       - stay wizard even when wizard_just_friend is set - can apply a request \n\
       \"sleep=xx\" to sleep xx seconds after a request (useful e.g. to have \n\
       time to look at the memory used) His changes in database are not \n\
       recorded in the history. The variable must contain the wizard’s user \n\
       name."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let moderator_file =
    let doc =
      "Wizards moderators file. List of wizards moderating the database forum. \n\
       If empty or empty file, no moderation. One wizard name by line."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let supervisor =
    let doc =
      "Forum supervisor. The forum is not moderated but controlled afterwards. \n\
       This supervisor is a wizard who can delete any forum message. He can \n\
       see the real wizards or friend user names (since they can be hidden in \n\
       the \"ident\" area) and the IP address the message comes from."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let forum_exclude_file =
    let doc = "File recording a black list for the database forum." in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let welcome_logo =
    let doc = "Welcome logo path." in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let welcome_logo_style =
    let doc =
      "Welcome logo. The designated file should be in \n\
       bases/src/<basename>/images Called with <img \
       src=\"%prefix;m=IM;v=welcome_logo\" style=\"welcome_logo_style\">."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let css_prop =
    let doc =
      "The file stylesheet.css must be in the directory \"css\". Customize the \
       default.css stylesheet or create your own one and specify it to geneweb \
       by adding this line in your gwf. More about how to customize here : \
       https://geneweb.tuxfamily.org/wiki/CSS."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let highlight_color =
    let doc =
      "Highlight color used to highlight only a few text strings like the date \
       strings in Anniversaries pages. TODO: what are the other elements ?"
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let body_prop =
    let doc =
      "despite new css_prop, there is still usage of body_prop as per issue \
       https://github.com/geneweb/geneweb/issues/1696 old parameter since \
       version 3.08, defaut is empty. Refer to authorised attributes and those \
       replaced by css in \
       https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body"
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let short_place_threshold =
    let doc =
      "On Places/suname page do not display \"Full display\" list of places ff \
       more than short_place_threshold places."
    in
    mk ~default:500 ~doc __FUNCTION__ S.int

  let max_ancestor_implex =
    let doc =
      "Maximum number to see an identical ancestor in the surname list."
    in
    mk ~default:5 ~doc __FUNCTION__ S.int

  let long_date =
    let doc =
      "Display the date with the explicit day in parentheses. Only works if \
       the date is “exact” and *not* enter in “text mode”."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let fast_alphabetic =
    let doc =
      "Fast access to surnames and first names alphabetic order be fast \
       (interesting for very big databases). Drawback: the first page won’t \
       display the number of surnames or first names."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let allowed_titles_file =
    let doc = "File for allowed titles/domains (empty => all allowed)." in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let denied_titles_file =
    let doc = "After above, file for denied titles/domains." in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let print_advanced_title =
    let doc =
      "Print fieldhe title of individual in an advanced way. Default prints \
       only first_name and surname. In the advanced way, search for the title \
       name, estate ... and print something clever."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let notify_change =
    let doc =
      "Notify change program (e.g. shell script) to be executed for each \
       database change. First argument is the name of the base, then the \
       individual and finally the action performed. If you want to test a \
       specific change, such as delete an individual, you should test the \
       fourth argument as equal to dp (delete person). You can check the list \
       of possible modifications in the updhist.txt file (see update_text)."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let vowels =
    let doc =
      "In French, de Amélie should be written as d'Amélie because the string \
       Amélie begins by a vowel. The test is performed on a unaccented and \
       lowered copy of the string The vowel parameter provides a list of \
       characters considered as vowels Fancy utf-8 characters such as æ and ø \
       are taken into account The default value is aeiouy."
    in
    mk ~default:"aeiouy" ~doc __FUNCTION__ S.string

  let propose_alias =
    let doc = "Hide alias field proposed by default in person update form." in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let propose_qualifier =
    let doc =
      "Hide qualifier field proposed by default in person update form."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let default_image_no =
    let doc =
      "for modules/arbre_descendants.txt define a character string to be used \
       in place of missing portrait image. Note that in many other files this \
       is hardcoded to '?' or '&nbsp;' There is no default here."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let dump_bad_images =
    let doc =
      "for carrousel.txt dump sent image, if detected as empty type, in a \
       'bad-image' file old parameter since version 4.02, defaut is to ignore."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let has_events =
    let doc =
      "For chronologie module, By default test for events beyond birth, \
       baptism, mariage, death and burial to generate an events list. \
       Nevertheless the has_events var may force list creation or not may be \
       set to \"always\" or \"never\"."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let event_tooltips =
    let doc =
      "Display notes and sources as tooltips on NMBDS events (birth, baptism, \
       marriage, death, burial) in individual page header. Enabled by default. \
       Set to \"no\" to disable these tooltips."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let notes_alias_file =
    let doc =
      "for wiki syntax since version 5.00, TODO what purpose ? notes alias \
       file is <basename>.gwb/notes.alias by default, or specified here"
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let robot_index =
    let doc =
      "By default generated web pages have robots content=\"none\" except if \
       robot_index=yes defined since PR #1728 in version 7.1 There is a \
       similar robot_index_forum for forum pages."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let robot_index_forum =
    let doc =
      "By default generated web pages have robots content=\"none\" except if \
       robot_index=yes defined since PR #1728 in version 7.1 There is a \
       similar robot_index_forum for forum pages."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let plugins =
    let doc =
      "list of plugins to activate in this base, from previously loaded gwd \
       plugins since version 7.0. Details on \
       https://geneweb.tuxfamily.org/wiki/plugins"
    in
    mk ~default:[] ~doc __FUNCTION__ S.(list string)

  let roglo =
    let doc = "Activates Roglo-specific template, not active by defaut." in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let alt_serv_adr =
    let doc =
      "Alternative Geneweb server/base If set, display a button in home bar to \
       jump to another Geneweb base. Used to link related bases (e.g. \
       private/public, test/production). Alternative server address \
       (optional). If empty, use current server address."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let alt_serv_base =
    let doc = "Alternative base name (mandatory to enable the button)." in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let alt_serv_title =
    let doc =
      "Optional title for the alternative base (used in tooltip). If empty, \
       base name is used."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let red_if_not_exist =
    let doc =
      "red_if_not_exist forces links to appear as existing even if person is \
       missing or not accessible. Breaks red/blue link semantics. LEGACY: Kept \
       for backward compatibility. Do not use."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let ptempl =
    let doc =
      "in gwd/request.ml use Perso.interp_templ or person_selected if var is \
       set LEGACY: kept for backward compatibility, purpose unclear..."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let multi_parents =
    let doc =
      "in etc/updfam.txt legacy option to handle multiple parents (>2) DEV: do \
       not use in new setups."
    in
    mk ~deprecated:true ~default:false ~doc __FUNCTION__ S.bool

  let css =
    let doc =
      "for templm, identify css file to be used, either css.css display on grey\n\
      \  background (default) or css1.css display on black background if \
       variable\n\
      \  is set. TODO also present in potentially obsolete file anctree_h7.txt."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let datalist_fn = mk ~default:None __FUNCTION__ S.(option int)
  let datalist_sn = mk ~default:None __FUNCTION__ S.(option int)
  let datalist_occu = mk ~default:None __FUNCTION__ S.(option int)
  let datalist_place = mk ~default:None __FUNCTION__ S.(option int)
  let datalist_src = mk ~default:None __FUNCTION__ S.(option int)

  let datalist_book =
    let doc =
      "for templm, above automatic completion lists are generated from cache, \
       or from associated base books if variable is set."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option int)

  let event_age =
    let doc =
      "for templm, on individual page, display parents age at birthday, and \
       individual age for each event. (not the case by default)."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let jquery =
    let doc =
      "for templm, jquery functions: on individual page, display numbers of \
       ascendants, descendants, implexes on individual & family forms, search \
       for places, sources, occupations and search for witnesses if needed you \
       may disable all of that by variable set to 'no'."
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let maxlev =
    let doc =
      "for templm, number of descendant generations in person page, only 1 by\n\
      \  default (provided value is limited to max_desc_level)."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let max_dates =
    let doc =
      "for templm, for statistics (anclist.txt & deslist.txt) deactivate the \
       shortest path links if more than 200 dates (default value)."
    in
    mk ~default:200 ~doc __FUNCTION__ S.int

  let menu =
    let doc =
      "for templm, on individual page, some pulldown menus on top of page may \
       be changed as static menus at page bottom if variable is set."
    in
    mk ~default:1 ~doc __FUNCTION__ S.int

  let misc =
    let doc =
      "for templm, on individual page, if variable is set then list all \
       identifications of this individual and related wiki text. if not set \
       here, user has a button to togle display."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let move_comment =
    let doc =
      "for templm, In family form, if variable is set if no mariage comment, \
       then union comment is moved to mariage comment."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let particles =
    let doc =
      "for templm, In form pages, if uppercase is set, then able to change the \
       list of particles that will not be converted to uppercase. by default \
       the list is hardcoded in two files etc/templm/css.txt & \
       etc/templm/js_upd.txt"
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let setup_link =
    let doc =
      "for templm, on individual page, the gwsetup menu may be disabled"
    in
    mk ~default:true ~doc __FUNCTION__ S.bool

  let setup_http =
    let doc =
      "for templm, default gwsetup server url http://127.0.0.1:2316/ may be \
       changed here."
    in
    mk ~default:None ~doc __FUNCTION__ S.(option string)

  let show_flag =
    let doc =
      "for templm, if needed add country flags in list of languages to be \
       selected."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let timeline =
    let doc =
      "for templm, on individual page, if variable is set then add timeline as \
       detailed in etc/templm/timeline.txt"
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let uppercase =
    let doc =
      "for templm, In form pages, convert firstname lastname to firstname \
       LASTNAME."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool

  let wide =
    let doc =
      "for templm, display individual page on two columns by default or to \
       only one if variable is set if not set, then togle checkbox accessible \
       on individual page."
    in
    mk ~default:false ~doc __FUNCTION__ S.bool
end

module Format = struct
  module M = Map.Make (String)

  type any_field = Field : 'a field -> any_field
  type t = any_field M.t

  let empty = M.empty
  let add (f : 'a field) = M.add f.name (Field f)
  let find = M.find

  let pp ppf t =
    let pp_field ppf (_name, Field f) = pp_field ppf f in
    let sep ppf () = Format.pp_force_newline ppf () in
    Fmt.(iter_bindings ~sep M.iter pp_field) ppf t

  let v1 =
    (empty
    |> add Fields.access_by_key
    |> add Fields.show_consang
    |> add Fields.public_if_titles
    |> add Fields.public_if_no_date
    |> add Fields.display_sosa
    |> add Fields.full_sibling
    |> add Fields.default_image
    |> add Fields.visitor_access
    |> add Fields.no_image_for_visitor
    |> add Fields.no_note_for_visitor
    (* |> add Fields.friend_passwd *)
    (* |> add Fields.friend_passwd_file *)
    (* |> add Fields.wizard_passwd *)
    |> add Fields.wizard_just_friend
    |> add Fields.authorized_wizards_notes
    |> add Fields.hide_private_names
    |> add Fields.use_restrict
    |> add Fields.can_send_image
    |> add Fields.propose_titles
    |> add Fields.propose_add_family
    |> add Fields.propose_place_surnames
    |> add Fields.hide_advanced_request
    |> add Fields.counter
    |> add Fields.places_inverted
    |> add Fields.place_surname_link_to_ind
    (* |> add Fields.max_rlm_nbr *)
    (* |> add Fields.max_anc_level *)
    |> add Fields.max_anc_tree
    (* |> add Fields.max_desc_level *)
    (* |> add Fields.max_desc_tree *)
    (* |> add Fields.max_cousins *)
    (* |> add Fields.max_cousins_level *)
    |> add Fields.cache_cousins_tool
    (* |> add Fields.cache_cousins_ttl *)
    (* |> add Fields.latest_event *)
    |> add Fields.always_surname
    |> add Fields.history
    |> add Fields.history_diff
    (* |> add Fields.history_path *)
    |> add Fields.datalist_fnames
    |> add Fields.datalist_places
    |> add Fields.datalist_occupations
    |> add Fields.datalist_sources
    |> add Fields.datalist_pub_names
    |> add Fields.datalist_qualifiers
    |> add Fields.datalist_aliases
    |> add Fields.datalist_titles
    |> add Fields.datalist_estates
    |> add Fields.rpc_datalist
    |> add Fields.rpc_server_url
    |> add Fields.renamed
    |> add Fields.moved
    |> add Fields.gzip_html_compression
    |> add Fields.template
    |> add Fields.expand_env
    |> add Fields.perso_module_a
    |> add Fields.perso_module_b
    |> add Fields.perso_module_c
    |> add Fields.perso_module_d
    |> add Fields.perso_module_e
    |> add Fields.perso_module_f
    |> add Fields.perso_module_g
    |> add Fields.perso_module_h
    |> add Fields.perso_module_i
    |> add Fields.perso_module_j
    |> add Fields.perso_module_k
    |> add Fields.perso_module_l
    |> add Fields.perso_module_m
    |> add Fields.perso_module_n
    |> add Fields.perso_module_o
    |> add Fields.perso_module_p
    |> add Fields.perso_module_q
    |> add Fields.perso_module_r
    |> add Fields.perso_module_s
    |> add Fields.perso_module_t
    |> add Fields.perso_module_u
    |> add Fields.perso_module_v
    |> add Fields.perso_module_w
    |> add Fields.perso_module_x
    |> add Fields.perso_module_y
    |> add Fields.perso_module_z
    |> add Fields.occu_in_homonyms
    |> add Fields.p_mod
    |> add Fields.book_max_results
    |> add Fields.chk_data_max_results
    |> add Fields.max_nb_update
    |> add Fields.use_cdn
    |> add Fields.hide_querytime_bugs
    |> add Fields.origin_file
    |> add Fields.can_kill_ancestors
    |> add Fields.disable_forum
    |> add Fields.hide_connection
    |> add Fields.manitou
    |> add Fields.moderator_file
    |> add Fields.supervisor
    |> add Fields.forum_exclude_file
    |> add Fields.welcome_logo
    |> add Fields.welcome_logo_style
    |> add Fields.css_prop
    |> add Fields.highlight_color
    |> add Fields.body_prop
    |> add Fields.short_place_threshold
    |> add Fields.max_ancestor_implex
    |> add Fields.long_date
    |> add Fields.fast_alphabetic
    |> add Fields.allowed_titles_file
    |> add Fields.denied_titles_file
    |> add Fields.print_advanced_title
    |> add Fields.notify_change
    |> add Fields.vowels
    |> add Fields.propose_alias
    |> add Fields.propose_qualifier
    |> add Fields.default_image_no
    |> add Fields.dump_bad_images
    |> add Fields.has_events
    |> add Fields.event_tooltips
    |> add Fields.notes_alias_file
    |> add Fields.robot_index
    |> add Fields.robot_index_forum
    |> add Fields.plugins
    |> add Fields.roglo
    |> add Fields.alt_serv_adr
    |> add Fields.alt_serv_base
    |> add Fields.alt_serv_title
    |> add Fields.red_if_not_exist
    |> add Fields.ptempl
    |> add Fields.multi_parents
    |> add Fields.css
    |> add Fields.datalist_fn
    |> add Fields.datalist_sn
    |> add Fields.datalist_occu
    |> add Fields.datalist_place
    |> add Fields.datalist_src
    |> add Fields.datalist_book
    |> add Fields.event_age
    |> add Fields.jquery
    |> add Fields.maxlev
    |> add Fields.max_dates
    |> add Fields.menu
    |> add Fields.misc
    |> add Fields.move_comment
    |> add Fields.particles
    |> add Fields.setup_link
    |> add Fields.setup_http
    |> add Fields.show_flag
    |> add Fields.timeline
    |> add Fields.uppercase
    |> add Fields.wide)
  [@ocamlformat "disable"]

  let latest = v1
end

module M = Types.Map.Make (struct
  type 'a t = 'a field

  let type_of { wit; _ } = wit
  let compare = compare
end)

type format = Format.t
type t = M.t
type any_value = Value : 'a field * 'a -> any_value | Ignore : any_value

module Parser : sig
  val any_value : format -> any_value Angstrom.t
end = struct
  open Angstrom

  let bool =
    let true_ = (string "yes" <|> string "on") *> return true in
    let false_ = (string "no" <|> string "off") *> return false in
    choice [ true_; false_ ]

  let int =
    let* s =
      take_while (fun c -> match c with '0' .. '9' -> true | _ -> false)
    in
    return @@ int_of_string s

  let string_no_ws =
    take_till (fun c -> match c with ' ' | '\t' -> true | _ -> false)

  let rec value : type a. a Types.t -> a t =
   fun wit ->
    match wit with
    | Bool -> bool
    | Int -> int
    | String -> string_no_ws
    | Option w -> optional w
    | List w -> list w
    | _ -> failwith "unsupported type"

  and optional : type a. a Types.t -> a option t =
   fun wit -> option Option.none (map (value wit) ~f:Option.some)

  and list : type a. a Types.t -> a list t =
   fun wit ->
    let comma = char ',' in
    sep_by comma (value wit)

  let ws = skip_while (fun c -> match c with ' ' | '\t' -> true | _ -> false)
  let nl = string "\n" <|> string "\r\n"

  let comment =
    let content =
      skip_while (fun c -> match c with '\n' -> false | _ -> true)
    in
    ws *> char '#' *> content <* nl

  let empty_line = map (ws *> nl) ~f:ignore
  let ignore = comment <|> empty_line

  let value fmt =
    let* k = ws *> take_while (Char.equal '=') <* ws <* char '=' in
    match Format.find k fmt with
    | exception Not_found -> assert false
    | Format.Field f ->
        let* v = ws *> value f.wit <* ws in
        return (Value (f, v))

  let any_value fmt =
    choice [ map ignore ~f:(fun () -> Ignore); map (value fmt) ~f:Fun.id ]
end

let read ic fmt =
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> acc
    | line -> (
        match
          Angstrom.parse_string ~consume:All (Parser.any_value fmt) line
        with
        | Ok (Value (f, v)) -> loop @@ M.add f v acc
        | Ok Ignore -> loop acc
        | Error _ -> assert false)
  in
  loop M.empty

let field f t =
  match M.find f t with exception Not_found -> f.default | v -> v
