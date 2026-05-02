type t =
  | Null
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | List of t list
  | Assoc of (string * t) list

let null = Null
let bool value = Bool value
let int value = Int value
let int64 value = Int64 value
let float value = Float value
let string value = String value
let list values = List values
let assoc values = Assoc values
