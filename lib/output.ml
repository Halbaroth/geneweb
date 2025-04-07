open Config

let status conf s = conf.output_conf.status s
let header conf fmt = Printf.ksprintf conf.output_conf.header fmt
let print_sstring conf s = conf.output_conf.body s

let print_string conf
    (s : [< `encoded | `escaped | `safe ] Geneweb_sanatize.Sanatize.astring) =
  conf.output_conf.body ((s :> Geneweb_sanatize.Sanatize.safe_string) :> string)

let printf conf fmt = Printf.ksprintf conf.output_conf.body fmt
let flush conf = conf.output_conf.flush ()
