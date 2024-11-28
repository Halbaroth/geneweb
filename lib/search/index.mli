module type S = sig
  type 'a t
  type char_
  type word

  val empty : 'a t
  val cardinal : 'a t -> int
  val lookup : word -> 'a t -> (word * 'a) Seq.t
  val insert : word -> 'a -> 'a t -> 'a t
  val remove : word -> 'a t -> 'a t
  val fold : (word -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (word -> 'a -> unit) -> 'a t -> unit
  val to_seq : 'a t -> (word * 'a) Seq.t
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val pp_statistics : 'a t Fmt.t
end

module Make (W : Word.S) : S with type char_ = W.char_ and type word = W.t

module Default : S with type char_ = char and type word = string
