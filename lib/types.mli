module Mode : sig
  type t =
    | Online
    | Offline
    | Disabled
    | Shared

  val to_string : t -> string
  val of_string : string -> (t, Error.t) Stdlib.result
end

module Resume : sig
  type t =
    | Allow
    | Never
    | Must
    | Auto

  val to_string : t -> string
  val of_string : string -> (t, Error.t) Stdlib.result
end

module Reinit : sig
  type t =
    | Return_previous
    | Finish_previous

  val to_string : t -> string
end

module Alert : sig
  type level =
    | Info
    | Warn
    | Error

  val to_string : level -> string
end
