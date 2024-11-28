module type S = sig
  type char_
  type t

  val empty : t
  val of_list : char_ list -> t
  val length : t -> int
  val get : t -> int -> char_
  val cat : t -> t -> t
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
  let cat = String.cat
  let compare_char = Char.compare
  let pp = Fmt.string
end
