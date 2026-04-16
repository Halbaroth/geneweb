(** This module provides type-aware functions for encoding and decoding JSON
    messages. *)

type (_, _) eq = Equal : ('a, 'a) eq
type _ id
type 'a generic

(* FIXME: The type [!'a t] would be more precise, but OCaml 4.08 does not
   support the injectivity annotation. *)
type 'a t = private
  | Unit : unit t
  | Bool : bool t
  | Float : float t
  | Int : int t
  | String : string t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Option : 'a t -> 'a option t
  | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Generic : 'a generic -> 'a t

type error = [ `Json_error of string | `Invalid_type of string ]

val pp_error : error Fmt.t
(** [pp_error ppf e] prints the error message for logs. *)

val make :
  name:string ->
  compare:('a -> 'a -> int) ->
  of_json:(Yojson.Safe.t -> ('a, error) result) ->
  to_json:('a -> Yojson.Safe.t) ->
  pp:'a Fmt.t ->
  unit ->
  'a t
(** [make ~compare ~to_json ~of_jon ~pp] creates a type witness for the type
    ['a] using the encoder and decoder given as argument. [pp] is used to print
    this encoding in [pp]. *)

val uid : 'a t -> int
(** Unique identifier per witness type. *)

val equal : 'a t -> 'b t -> ('a, 'b) eq option
(** Provable equality between type witnesses. *)

val compare : 'a t -> 'b t -> int
val compare_values : 'a t -> 'b t -> 'a -> 'b -> int

val of_json : 'a t -> Yojson.Safe.t -> ('a, error) result
(** [of_json t j] returns the value of type 'a that is encoded in [j] or a
    string error if [j] does not encode such a value. *)

val to_json : 'a t -> 'a -> Yojson.Safe.t
(** [to_json t v j] returns the JSON representation of the value [v]. *)

val pp : 'a t Fmt.t
(** [pp ppf t] prints the type witness [t] for debugging purposes. *)

val pp_value : 'a t -> 'a Fmt.t

module Syntax : sig
  val unit : unit t
  (** Type witness of unit. *)

  val bool : bool t
  (** Type witness of bool. *)

  val float : float t
  (** Type witness of float. *)

  val int : int t
  (** Type witness of int. *)

  val string : string t
  (** Type witness of string. *)

  val list : 'a t -> 'a list t
  (** Type witness of list. *)

  val array : 'a t -> 'a array t
  (** Type witness of array. *)

  val option : 'a t -> 'a option t
  (** Type witness of option. *)

  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  (** Type witness of pair. *)

  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Type witness of triple. *)

  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** Type witness of quadruple. *)
end

type 'a wit = 'a t

module Map : sig
  module type Key = sig
    type 'a t

    val type_of : 'a t -> 'a wit
    val compare : 'a t -> 'a t -> int
  end

  module type S = sig
    type 'a key
    type t

    val empty : t
    val add : 'a key -> 'a -> t -> t
    val find : 'a key -> t -> 'a
  end

  module Make (K : Key) : S with type 'a key = 'a K.t
end
