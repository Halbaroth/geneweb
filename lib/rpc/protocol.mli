type t
type 'a val_

val mk : string -> 'a Encoding.desc -> 'a -> 'a val_
val add : 'a val_ -> t -> t
val find : t -> string -> 'a Encoding.desc -> 'a option
val pingpong : t
