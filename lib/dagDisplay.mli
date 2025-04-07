(* TODOOCP *)
val image_txt :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Geneweb_sanatize.Sanatize.safe_string

type item = Item of Gwdb.person * Geneweb_sanatize.Sanatize.safe_string

val make_tree_hts :
  Config.config ->
  Gwdb.base ->
  (Gwdb.person -> item) ->
  (Gwdb.iper -> Geneweb_sanatize.Sanatize.escaped_string) ->
  bool ->
  Gwdb.iper list ->
  (Gwdb.iper * (Gwdb.iper * Gwdb.ifam option)) list ->
  (Gwdb.iper, 'a) Def.choice Dag2html.dag ->
  (int
  * Dag2html.align
  * Geneweb_sanatize.Sanatize.safe_string Dag2html.table_data)
  array
  array
(** [make_tree_hts conf base elem_txt vbar_txt invert set spl d]  *)

val print_slices_menu_or_dag_page :
  Config.config ->
  Gwdb.base ->
  Geneweb_sanatize.Sanatize.safe_string ->
  (int
  * Dag2html.align
  * Geneweb_sanatize.Sanatize.safe_string Dag2html.table_data)
  array
  array ->
  Geneweb_sanatize.Sanatize.escaped_string ->
  unit
(** [print_slices_menu_or_dag_page conf page_title hts next_txt] *)

val make_and_print_dag :
  Config.config ->
  Gwdb.base ->
  (Gwdb.person -> item) ->
  (Gwdb.iper -> Geneweb_sanatize.Sanatize.escaped_string) ->
  bool ->
  Gwdb.iper list ->
  (Gwdb.iper * (Gwdb.iper * Gwdb.ifam option)) list ->
  Geneweb_sanatize.Sanatize.safe_string ->
  Geneweb_sanatize.Sanatize.escaped_string ->
  unit
(** [make_and_print_dag conf base elem_txt vbar_txt invert set spl page_title next_txt] *)

val print : Config.config -> Gwdb.base -> unit
