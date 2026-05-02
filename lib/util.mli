val todo : string -> 'a

val default_base_url : string
val starts_with : prefix:string -> string -> bool
val opt_first : 'a option -> 'a option -> 'a option
val split_comma : string -> string list
val getenv : ?environ:(string * string) list -> string -> string option
val value_to_yojson : Value.t -> Yojson.Safe.t
