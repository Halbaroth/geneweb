(** This module provides a specialization of the trie structure for full-text
    engine. More precisely, it constructs a reverse index of sentences with
    location. *)

type loc = { offset : int; len : int }

val compare_loc : loc -> loc -> int

module type S = sig
  type t
  type word

  val empty : t
  (** The empty reverse index. *)

  val add : word -> word * loc -> t -> t
  (** [add w s loc idx] binds the word [w] with the sentence [s] and the
      location [loc] in [idx]. It means that [s] contains exactly the
      word [w] at the location [loc]. *)

  val search : word -> t -> (word * loc Seq.t) Seq.t
  (** [search w idx] returns the sequence of all the sentences in the index
      [idx] with a sequence of locations of blabla. *)

  val pp_statistics : t Fmt.t
end

module Make (W : Word.S) : S with type word = W.t

module Default : S with type word = string
