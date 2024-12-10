module type S = sig
  type 'a t
  type char_
  type word

  val empty : 'a t
  (** An empty trie. *)

  val of_list : (word * 'a) list -> 'a t
  (** Create an trie from an associative list. *)

  val cardinal : 'a t -> int
  (** Return the cardinal of the trie, that is its number of elements. *)

  val mem : word -> 'a t -> bool
  (** [mem w t] checks if the word [w] is present in [t]. *)

  val fuzzy_mem : max_dist:int -> word -> 'a t -> bool
  (** [fuzzy_mem ~max_dist w t] checks if there is a word in [t] at distance at
      most [max_dist] of [w]. *)

  val add : word -> 'a -> 'a t -> 'a t
  (** [add w t v] adds the word [w] with the value [v] in [t].
      If [w] was already present in [t], its previous value is replaced by
      [v]. *)

  val update : word -> ('a option -> 'a option) -> 'a t -> 'a t

  val remove : word -> 'a t -> 'a t
  (** [remove w t] removes the word [w] if it is present in [t]. *)

  val search : word -> 'a t -> (word * 'a) Seq.t
  (** [search w t] returns the sequence in lexicographic order of all the
      words in [t] with [w] as prefix. *)

  val fuzzy_search : max_dist:int -> word -> 'a t -> (word * 'a) Seq.t

  val fold : (word -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (word -> 'a -> unit) -> 'a t -> unit

  val to_seq : 'a t -> (word * 'a) Seq.t

  val pp : 'a Fmt.t -> 'a t Fmt.t
  (** Prints all the binding in the trie in lexicographic order. *)

  val pp_statistics : 'a t Fmt.t
  (** Prints statistics information for debugging. *)
end

module Make (W : Word.S) : S with type char_ = W.char_ and type word = W.t

module Default : S with type char_ = char and type word = string
