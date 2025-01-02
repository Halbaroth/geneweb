(** This module provides an implementation of reversed index based on
    trie data structures. *)

module type S = sig
  type t
  type char_
  type word
  type entry

  val of_seq : (word * entry) Seq.t -> t

  val search : word list -> t -> entry Seq.t
  (** [search ws t] returns the sequence of entries in the index [t] which
      are associated with all the exact words [ws]. *)

  val search_prefix : word list -> t -> entry Seq.t
  (** [search_prefix ps t] returns the sequence of entries in the index [t]
      which are associated with all the prefix [ps]. *)
end

module type Entry = sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val hash : t -> int
  val pp : t Fmt.t
end

module Make (W : Geneweb_structures.Word.S) (E : Entry) :
  S with type char_ = W.char_ and type word = W.t and type entry = E.t

type 'a loc = { content : 'a; offset : int; len : int }

module Default :
  S with type char_ = char and type word = string and type entry = string loc
