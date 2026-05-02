module Mode = struct
  type t =
    | Online
    | Offline
    | Disabled
    | Shared

  let to_string = function
    | Online -> "online"
    | Offline -> "offline"
    | Disabled -> "disabled"
    | Shared -> "shared"

  let of_string = function
    | "online" -> Ok Online
    | "run" -> Ok Online
    | "offline" -> Ok Offline
    | "dryrun" -> Ok Offline
    | "disabled" -> Ok Disabled
    | "shared" -> Ok Shared
    | value ->
      Error
        (Error.make Error.Usage
           (Printf.sprintf "unknown W&B mode: %S" value))
end

module Resume = struct
  type t =
    | Allow
    | Never
    | Must
    | Auto

  let to_string = function
    | Allow -> "allow"
    | Never -> "never"
    | Must -> "must"
    | Auto -> "auto"

  let of_string = function
    | "allow" -> Ok Allow
    | "never" -> Ok Never
    | "must" -> Ok Must
    | "auto" -> Ok Auto
    | value ->
      Error
        (Error.make Error.Usage
           (Printf.sprintf "unknown W&B resume policy: %S" value))
end

module Reinit = struct
  type t =
    | Return_previous
    | Finish_previous

  let to_string = function
    | Return_previous -> "return_previous"
    | Finish_previous -> "finish_previous"
end

module Alert = struct
  type level =
    | Info
    | Warn
    | Error

  let to_string = function
    | Info -> "info"
    | Warn -> "warn"
    | Error -> "error"
end
