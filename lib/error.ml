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

let make ?(retryable = false) ?cause kind message =
  { kind; message; retryable; cause }

let kind t = t.kind
let message t = t.message
let retryable t = t.retryable

let string_of_kind = function
  | Authentication -> "authentication"
  | Communication -> "communication"
  | Usage -> "usage"
  | Io -> "io"
  | Serialization -> "serialization"
  | Timeout -> "timeout"
  | Cancelled -> "cancelled"
  | Backend -> "backend"
  | Unsupported -> "unsupported"
  | Unknown -> "unknown"

let to_string t =
  Printf.sprintf "%s: %s" (string_of_kind t.kind) t.message

let pp fmt t =
  Format.pp_print_string fmt (to_string t)
