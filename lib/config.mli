type t

val empty : t
val of_list : (string * Value.t) list -> t
val get : t -> string -> Value.t option
val set : t -> string -> Value.t -> t
val update : t -> (string * Value.t) list -> t
val keys : t -> string list
val bindings : t -> (string * Value.t) list
val of_json : 'json -> (t, Error.t) Stdlib.result
val of_json_file : string -> (t, Error.t) Stdlib.result
