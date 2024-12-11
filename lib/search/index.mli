(** This module provides a specialization of the trie structure for full-text
    engine. More precisely, it constructs a reverse index of sentences with
    location. *)

module type S = sig
  type t
  type word

  val empty : t
  val add : word -> word -> t -> t
  val search : word list -> t -> word list
end

module Make (W : Word.S) : S with type word = W.t
module Default : S with type word = string
