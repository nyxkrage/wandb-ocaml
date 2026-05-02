val url_netloc : string -> string option
val read_file : string -> (string, Error.t) Stdlib.result
val validate_api_key : string -> (string, Error.t) Stdlib.result

val authenticate :
  base_url:string ->
  mode:Types.Mode.t ->
  force:bool ->
  explicit_api_key:string option ->
  (string option, Error.t) Stdlib.result
