type kind =
  | Authentication
  | Communication
  | Usage
  | Io
  | Serialization
  | Timeout
  | Cancelled
  | Backend
  | Unsupported
  | Unknown

type t =
  { kind : kind
  ; message : string
  ; retryable : bool
  ; cause : exn option
  }

exception Wandb_error of t

val make : ?retryable:bool -> ?cause:exn -> kind -> string -> t
val kind : t -> kind
val message : t -> string
val retryable : t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit
