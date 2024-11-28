let is_available = false

type _ ancient = unit

let error_msg =
  "Called the dummy implementation of gw_ancient. Please install the ancient \
   library with `opam install ancient`"

let mark _ = failwith error_msg
let follow _ = failwith error_msg
let delete _ = failwith error_msg
