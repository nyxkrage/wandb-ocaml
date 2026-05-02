type t

module Status : sig
  type t =
    | Running
    | Finished
    | Failed
    | Crashed
    | Killed
    | Offline
    | Disabled
    | Unknown of string
end

val init :
  ?entity:string ->
  ?project:string ->
  ?dir:string ->
  ?id:string ->
  ?name:string ->
  ?notes:string ->
  ?tags:string list ->
  ?config:Config.t ->
  ?config_exclude_keys:string list ->
  ?config_include_keys:string list ->
  ?group:string ->
  ?job_type:string ->
  ?mode:Types.Mode.t ->
  ?force:bool ->
  ?resume:Types.Resume.t ->
  ?resume_from:string ->
  ?fork_from:string ->
  unit ->
  (t, Error.t) Stdlib.result

val create :
  id:string ->
  ?name:string ->
  ?entity:string ->
  ?project:string ->
  root_dir:string ->
  ?sync_dir:string ->
  ?tags:string list ->
  ?config:Config.t ->
  mode:Types.Mode.t ->
  base_url:string ->
  unit ->
  t

val id : t -> string
val name : t -> string option
val entity : t -> string option
val project : t -> string option
val tags : t -> string list
val config : t -> Config.t
val url : t -> (string option, Error.t) Stdlib.result
val sweep_url : t -> (string option, Error.t) Stdlib.result
val status : t -> (Status.t, Error.t) Stdlib.result

val log :
  ?step:int ->
  ?commit:bool ->
  t ->
  (string * Value.t) list ->
  (unit, Error.t) Stdlib.result

val finish : ?exit_code:int -> t -> (unit, Error.t) Stdlib.result
val set_core : t -> Core.t option -> unit
