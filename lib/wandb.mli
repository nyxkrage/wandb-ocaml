module Error : module type of Error
module Mode : module type of Types.Mode
module Resume : module type of Types.Resume
module Reinit : module type of Types.Reinit
module Value : module type of Value
module Settings : module type of Settings
module Config : module type of Config
module Alert : module type of Types.Alert
module Run : module type of Run

type 'a result = ('a, Error.t) Stdlib.result
type path = string

val version : string

val init :
  ?entity:string ->
  ?project:string ->
  ?dir:path ->
  ?id:string ->
  ?name:string ->
  ?notes:string ->
  ?tags:string list ->
  ?config:Config.t ->
  ?config_exclude_keys:string list ->
  ?config_include_keys:string list ->
  ?group:string ->
  ?job_type:string ->
  ?mode:Mode.t ->
  ?force:bool ->
  ?reinit:Reinit.t ->
  ?resume:Resume.t ->
  ?resume_from:string ->
  ?fork_from:string ->
  unit ->
  Run.t result

val with_run :
  ?entity:string ->
  ?project:string ->
  ?dir:path ->
  ?id:string ->
  ?name:string ->
  ?notes:string ->
  ?tags:string list ->
  ?config:Config.t ->
  ?config_exclude_keys:string list ->
  ?config_include_keys:string list ->
  ?group:string ->
  ?job_type:string ->
  ?mode:Mode.t ->
  ?force:bool ->
  ?resume:Resume.t ->
  ?resume_from:string ->
  ?fork_from:string ->
  f:(Run.t -> 'a result) ->
  unit ->
  'a result

val current_run : unit -> Run.t option
val log : ?step:int -> ?commit:bool -> (string * Value.t) list -> unit result
val finish : ?exit_code:int -> unit -> unit result



