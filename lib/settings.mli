type console =
  | Off
  | Wrap
  | Redirect

type anonymous =
  | Allow
  | Must
  | Never

type start_method =
  | Thread
  | Fork
  | Spawn
  | Forkserver

type t =
  { api_key : string option
  ; base_url : string option
  ; mode : Types.Mode.t option
  ; root_dir : string option
  ; entity : string option
  ; project : string option
  ; run_id : string option
  ; run_name : string option
  ; run_notes : string option
  ; run_tags : string list option
  ; run_group : string option
  ; run_job_type : string option
  ; resume : Types.Resume.t option
  ; anonymous : anonymous option
  ; console : console option
  ; quiet : bool option
  ; start_method : start_method option
  ; http_proxy : string option
  ; https_proxy : string option
  ; ignore_globs : string list option
  ; service_wait_s : float option
  ; verify_ssl : bool option
  }

val make :
  ?api_key:string ->
  ?base_url:string ->
  ?mode:Types.Mode.t ->
  ?root_dir:string ->
  ?run_id:string ->
  ?run_group:string ->
  ?run_job_type:string ->
  ?resume:Types.Resume.t ->
  ?anonymous:anonymous ->
  ?console:console ->
  ?quiet:bool ->
  ?start_method:start_method ->
  ?http_proxy:string ->
  ?https_proxy:string ->
  ?ignore_globs:string list ->
  ?service_wait_s:float ->
  ?verify_ssl:bool ->
  unit ->
  t

val empty : t
val default : unit -> t
val from_env : ?environ:(string * string) list -> unit -> t
val merge : t -> t -> t
val update : t -> (string * Value.t) list -> (t, Error.t) Stdlib.result
val parse_bool : string -> bool option
