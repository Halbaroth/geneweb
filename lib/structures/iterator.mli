exception End
(** Exception raised when attempting to access elements beyond the
    end of the iterator. *)

type ('a, 'cmp) t =
  < comparator : ('a, 'cmp) Comparator.t
  ; curr : unit -> 'a
  ; next : unit -> unit
  ; seek : 'a -> unit >
(** Type of an iterator parametrized by element type and comparator
    function.

    [it#curr ()] returns the element currently pointed by the iterator [it].
    @raise End if the iterator has reached the end of the collection.

    [it#next ()] advances the iterator [it] to the next element.

    [it#seek e] advances the iterator [it] at the smallest element in the
    collection that is greater or equal to [e]. If already positioned at this
    smallest element, the iterator remains unchanged. For a sequence of
    calls [it#seek ei] where e1 <= ... <= en, the amortized complexity of
    is expected to be O(1 + log(N/n)) where N is the cardinal of the set
    and n is the number of calls. *)

val equal : ('a, 'cmp) t -> ('a, 'cmp) t -> bool
(** [equal it1 it2] checks if two iterators [it1] and [it2] are equal.
    This function does not consume [it1] and [it2].

    This function consumes both [it1] and [it2]. *)

val union : ('a, 'cmp) t list -> ('a, 'cmp) t
(** [union cmp l] creates a new iterator whose the elements are the union of
    the elements of the iterators in [l]. The resulting iterator produces
    elements in ascending order based on [cmp] and consumes iterators of [l].

    @raise Invalid_argument if the list is empty. *)

val join : ('a, 'cmp) t list -> ('a, 'cmp) t
(** [join l] computes the join iterator of the iterators [l]. The resulting
    iterator produces elements in ascending order based on [cmp] and consumes
    iterators of [l].

    @raise Invalid_argument if the list is empty. *)

val to_seq : ('a, 'cmp) t -> 'a Seq.t
(** [to_seq it] converts the iterator [it] into a sequence. The
    resulting sequence consumes [it]. *)
