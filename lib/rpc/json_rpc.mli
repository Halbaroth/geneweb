(** Implementation of the JSON-RPC 2.0 Specification.
    See https://www.jsonrpc.org/specification for details. *)

type id = [ `String of string | `Int of int | `Null ]
type obj = (string * Yojson.Safe.t) list
type params = ByPosition of Yojson.Safe.t list | ByName of obj

(** Type of the request object. *)
type request = private
  | Request of id * string * params option
  | Notification of string * params option

type error = private {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}
(** Type of the error object. *)

(** Type of the response object. *)
type response = private Success of id * Yojson.Safe.t | Echec of id * error

module Error : sig
  type t = error

  val error : ?data:Yojson.Safe.t -> int -> string -> t
  (** [error ?data code message] creates an error object for the code [c]
      and the explanation [message]. *)

  val parse_error : ?data:Yojson.Safe.t -> unit -> t
  (** Invalid JSON was received by the server. An error occurred on the
    server while parsing the JSON text. *)

  val invalid_request : ?data:Yojson.Safe.t -> unit -> t
  (** The JSON sent is not a valid Request object. *)

  val method_not_found : ?data:Yojson.Safe.t -> unit -> t
  (** The method does not exist / is not available. *)

  val invalid_params : ?data:Yojson.Safe.t -> unit -> t
  (** Invalid method parameter(s). *)

  val internal_error : ?data:Yojson.Safe.t -> unit -> t
  (** Internal JSON-RPC error. *)

  val server_error : ?data:Yojson.Safe.t -> int -> string -> t
  (** Reserved for implementation-defined server-errors.

      @raise Failwith if the code is not between -32099 and -32000. *)

  val to_json : t -> Yojson.Safe.t
  (** [to_json e] converts the error object [e] into its JSON representation. *)

  val of_json : Yojson.Safe.t -> t option
  (** [of_json j] tries to recognize a JSON representation of an error object
      in the JSON value [j]. *)

  val pp : t Fmt.t
end

module Request : sig
  type t = request

  val request : ?params:params -> string -> id -> t
  (** [request ?params meth id] creates an request message for the
      method [meth] and the option parameter list [params]. *)

  val notification : ?params:params -> string -> t
  (** [notify ?params meth] creates a notify message for the method [meth]
      and the option parameter list [params]. *)

  val of_json : Yojson.Safe.t -> t option
  (** [of_json j] tries to recognize a JSON representation of a request or
      a notification in the JSON value [j]. A request must contain an
      id field and a notification must not contain such a field. *)

  val to_json : t -> Yojson.Safe.t
  (** [to_json t] converts the response [t] into its JSON representation. *)

  val pp : t Fmt.t
  (** [pp ppf t] prints the request [t] on the formatter [ppf] for debugging
      purposes. *)
end

module Response : sig
  type t = response
  type error

  val success : id -> Yojson.Safe.t -> t
  (** [result id c] creates a success response with the identifier [id]
      and the content [c]. *)

  val echec : id -> error -> t
  (** [echec id e] creates an echec response with the identifier [id] and
      the error object [e]. *)

  val id : t -> id
  (** [id t] returns the identifier of the response. *)

  val parse_error : ?data:Yojson.Safe.t -> unit -> t
  (** See [Error.parse_error]. According to the specification, the response
      must not contain an identifier. *)

  val invalid_request : ?data:Yojson.Safe.t -> unit -> t
  (** See [Error.invalid_request]. According to the specification, the response
      must not contain an identifier. *)

  val method_not_found : ?data:Yojson.Safe.t -> id -> t
  (** See [Error.method_not_found]. *)

  val invalid_params : ?data:Yojson.Safe.t -> id -> t
  (** See [Error.invalid_params]. *)

  val internal_error : ?data:Yojson.Safe.t -> id -> t
  (** See [Error.internal_error]. *)

  val server_error :
    ?data:Yojson.Safe.t ->
    code:int ->
    id ->
    ('a, Format.formatter, unit, t) format4 ->
    'a
  (** See [Error.server_error]. *)

  val of_json : Yojson.Safe.t -> t option
  (** [of_json j] attemps to recognize a JSON representation of a response
      in the JSON value [j]. *)

  val to_json : t -> Yojson.Safe.t
  (** [to_json t] converts the response [t] into its JSON representation. *)

  val pp : t Fmt.t
  (** [pp ppf t] prints the response [t] on the formatter [ppf] for debugging
      purposes. *)
end
