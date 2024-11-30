module type S = sig
  type char_
  type t

  val empty : t
  val of_list : char_ list -> t
  val length : t -> int
  val get : t -> int -> char_
  val ( ^ ) : t -> t -> t
  val suffix : int -> t -> t
  val compare_char : char_ -> char_ -> int
  val pp : t Fmt.t
end

module Default : S with type char_ = char and type t = string = struct
  type char_ = char
  type t = string

  let empty = ""
  let of_list l = String.of_seq @@ List.to_seq l
  let length = String.length
  let get = String.get
  let ( ^ ) = ( ^ )
  let suffix offset s = String.sub s offset (String.length s)

  let compare_char c1 c2 =
    Char.compare (Char.lowercase_ascii c1) (Char.lowercase_ascii c2)

  let pp = Fmt.string
end
