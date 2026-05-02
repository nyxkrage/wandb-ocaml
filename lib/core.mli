(** Connection to the wandb-core backend process. *)

type t

val default_core_path : string
(** Path to the wandb-core binary. Looks for WANDB_CORE environment variable,
    then falls back to [wandb-core] from PATH. *)

val start :
  ?core_path:string ->
  ?root_dir:string ->
  ?wandb_dir:string ->
  ?sync_dir:string ->
  ?sync_file:string ->
  ?log_dir:string ->
  ?log_internal:string ->
  ?log_user:string ->
  base_url:string ->
  api_key:string ->
  entity:string option ->
  project:string ->
  run_id:string ->
  ?run_name:string ->
  ?tags:string list ->
  ?notes:string ->
  ?run_mode:string ->
  ?mode:string ->
  unit ->
  (t, [> `Msg of string ]) result
(** Launch wandb-core, connect to its Unix socket, send inform_init,
    and return a connection handle. *)

val init_run :
  t ->
  id:string ->
  ?entity:string ->
  project:string ->
  ?name:string ->
  ?notes:string ->
  tags:string list ->
  config:Config.t ->
  stream_id:string ->
  unit ->
  (unit, [> `Msg of string]) result
(** Construct and send run record + run start, wait for confirmation. *)

val log :
  t ->
  stream_id:string ->
  values:(string * Value.t) list ->
  step:int option ->
  flush:bool ->
  unit
(** Construct and send a partial history record (fire-and-forget). *)

val finish_run :
  t ->
  stream_id:string ->
  exit_code:int ->
  (unit, [> `Msg of string]) result
(** Construct and send exit record, wait for response. *)

val finish : t -> exit_code:int -> stream_id:string -> unit
(** Send inform_finish and inform_teardown, then close the connection. *)
