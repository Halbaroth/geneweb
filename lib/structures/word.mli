module type S = sig
  type char_
  val compare_char : char_ -> char_ -> int

  type t

  val empty : t
  val of_rev_list : char_ list -> t
  val length : t -> int
  val compare : t -> t -> int
  val hash : t -> int
  val get : t -> int -> char_
  val cat : t -> t -> t
  val pp : t Fmt.t
end

module Default : S with type char_ = char and type t = string
