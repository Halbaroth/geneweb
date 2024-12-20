module type S = sig
  type elt
  (** Type of elements of the collection. *)

  type t
  (** Type of immutable set. *)

  val of_seq : elt Seq.t -> t
  (** Build an immutable set from a sequence of elements. The sequence is
      entirely forced while building the set. *)

  val to_seq : t -> elt Seq.t
  (** [to_seq t] returns the sequence of elements of [t] in ascending
      ordering. *)

  val mem : elt -> t -> bool
  (** [mem e t] checks if the element [e] is present in the set [t]. *)

  val cardinal : t -> int
  (** [cardinal t] returns the cardinal of the set. *)

  module Iterator : Iterator.S with type elt = elt

  val iterator : t -> Iterator.t
  (** [iterator t] creates a mutable iterator for the set [t] starting on the
      first element. This iterator cannot be invalidate as the set immutable. *)
end

module Make (O : Intf.Ordered) : S with type elt = O.t
