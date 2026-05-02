type t =
  | Null
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | List of t list
  | Assoc of (string * t) list

val null : t
val bool : bool -> t
val int : int -> t
val int64 : int64 -> t
val float : float -> t
val string : string -> t
val list : t list -> t
val assoc : (string * t) list -> t
