let ( let* ) = Result.bind

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

let empty =
  { api_key = None
  ; base_url = None
  ; mode = None
  ; root_dir = None
  ; entity = None
  ; project = None
  ; run_id = None
  ; run_name = None
  ; run_notes = None
  ; run_tags = None
  ; run_group = None
  ; run_job_type = None
  ; resume = None
  ; anonymous = None
  ; console = None
  ; quiet = None
  ; start_method = None
  ; http_proxy = None
  ; https_proxy = None
  ; ignore_globs = None
  ; service_wait_s = None
  ; verify_ssl = None
  }

let make ?api_key ?base_url ?mode ?root_dir ?run_id ?run_group
    ?run_job_type ?resume ?anonymous ?console ?quiet ?start_method
    ?http_proxy ?https_proxy ?ignore_globs ?service_wait_s ?verify_ssl () =
  { api_key
  ; base_url
  ; mode
  ; root_dir
  ; entity = None
  ; project = None
  ; run_id
  ; run_name = None
  ; run_notes = None
  ; run_tags = None
  ; run_group
  ; run_job_type
  ; resume
  ; anonymous
  ; console
  ; quiet
  ; start_method
  ; http_proxy
  ; https_proxy
  ; ignore_globs
  ; service_wait_s
  ; verify_ssl
  }

let default () =
  { empty with
    base_url = Some Util.default_base_url
  ; mode = Some Types.Mode.Online
  ; root_dir = Some (Sys.getcwd ())
  ; quiet = Some false
  ; ignore_globs = Some []
  ; service_wait_s = Some 30.0
  ; verify_ssl = Some true
  }

let parse_bool = function
  | "1" | "true" | "TRUE" | "True" | "yes" | "YES" | "on" | "ON" -> Some true
  | "0" | "false" | "FALSE" | "False" | "no" | "NO" | "off" | "OFF" -> Some false
  | _ -> None

let from_env ?environ () =
  let mode =
    match Util.getenv ?environ "WANDB_MODE" with
    | Some value -> Result.to_option (Types.Mode.of_string value)
    | None -> None
  in
  let quiet =
    match Util.getenv ?environ "WANDB_QUIET" with
    | Some value -> parse_bool value
    | None -> None
  in
  let verify_ssl =
    match Util.getenv ?environ "WANDB_VERIFY_SSL" with
    | Some value -> parse_bool value
    | None -> None
  in
  let service_wait_s =
    match Util.getenv ?environ "WANDB__SERVICE_WAIT" with
    | Some value -> (try Some (float_of_string value) with Failure _ -> None)
    | None -> None
  in
  { empty with
    api_key = Util.getenv ?environ "WANDB_API_KEY"
  ; base_url = Util.getenv ?environ "WANDB_BASE_URL"
  ; mode
  ; root_dir = Util.getenv ?environ "WANDB_DIR"
  ; entity = Util.getenv ?environ "WANDB_ENTITY"
  ; project = Util.getenv ?environ "WANDB_PROJECT"
  ; run_id = Util.getenv ?environ "WANDB_RUN_ID"
  ; run_name = Util.getenv ?environ "WANDB_NAME"
  ; run_notes = Util.getenv ?environ "WANDB_NOTES"
  ; run_tags = Option.map Util.split_comma (Util.getenv ?environ "WANDB_TAGS")
  ; run_group = Util.getenv ?environ "WANDB_RUN_GROUP"
  ; run_job_type = Util.getenv ?environ "WANDB_JOB_TYPE"
  ; quiet
  ; http_proxy = Util.getenv ?environ "WANDB_HTTP_PROXY"
  ; https_proxy = Util.getenv ?environ "WANDB_HTTPS_PROXY"
  ; ignore_globs = Option.map Util.split_comma (Util.getenv ?environ "WANDB_IGNORE_GLOBS")
  ; service_wait_s
  ; verify_ssl
  }

let merge left right =
  { api_key = Util.opt_first left.api_key right.api_key
  ; base_url = Util.opt_first left.base_url right.base_url
  ; mode = Util.opt_first left.mode right.mode
  ; root_dir = Util.opt_first left.root_dir right.root_dir
  ; entity = Util.opt_first left.entity right.entity
  ; project = Util.opt_first left.project right.project
  ; run_id = Util.opt_first left.run_id right.run_id
  ; run_name = Util.opt_first left.run_name right.run_name
  ; run_notes = Util.opt_first left.run_notes right.run_notes
  ; run_tags = Util.opt_first left.run_tags right.run_tags
  ; run_group = Util.opt_first left.run_group right.run_group
  ; run_job_type = Util.opt_first left.run_job_type right.run_job_type
  ; resume = Util.opt_first left.resume right.resume
  ; anonymous = Util.opt_first left.anonymous right.anonymous
  ; console = Util.opt_first left.console right.console
  ; quiet = Util.opt_first left.quiet right.quiet
  ; start_method = Util.opt_first left.start_method right.start_method
  ; http_proxy = Util.opt_first left.http_proxy right.http_proxy
  ; https_proxy = Util.opt_first left.https_proxy right.https_proxy
  ; ignore_globs = Util.opt_first left.ignore_globs right.ignore_globs
  ; service_wait_s = Util.opt_first left.service_wait_s right.service_wait_s
  ; verify_ssl = Util.opt_first left.verify_ssl right.verify_ssl
  }

let update settings values =
  let update_one settings (key, value) =
    match key, value with
    | "api_key", Value.String value -> Ok { settings with api_key = Some value }
    | "base_url", Value.String value -> Ok { settings with base_url = Some value }
    | "mode", Value.String value ->
        let* mode = Types.Mode.of_string value in
        Ok { settings with mode = Some mode }
    | "root_dir", Value.String value -> Ok { settings with root_dir = Some value }
    | "entity", Value.String value -> Ok { settings with entity = Some value }
    | "project", Value.String value -> Ok { settings with project = Some value }
    | "run_id", Value.String value -> Ok { settings with run_id = Some value }
    | "run_name", Value.String value -> Ok { settings with run_name = Some value }
    | "run_notes", Value.String value -> Ok { settings with run_notes = Some value }
    | "run_tags", Value.List values ->
        let rec collect acc = function
          | [] -> Ok (List.rev acc)
          | Value.String value :: rest -> collect (value :: acc) rest
          | _ -> Error (Error.make Error.Usage "run_tags must contain only strings")
        in
        let* run_tags = collect [] values in
        Ok { settings with run_tags = Some run_tags }
    | "run_group", Value.String value -> Ok { settings with run_group = Some value }
    | "run_job_type", Value.String value -> Ok { settings with run_job_type = Some value }
    | "resume", Value.String value ->
        let* resume = Types.Resume.of_string value in
        Ok { settings with resume = Some resume }
    | "quiet", Value.Bool value -> Ok { settings with quiet = Some value }
    | "http_proxy", Value.String value -> Ok { settings with http_proxy = Some value }
    | "https_proxy", Value.String value -> Ok { settings with https_proxy = Some value }
    | "ignore_globs", Value.List values ->
        let rec collect acc = function
          | [] -> Ok (List.rev acc)
          | Value.String value :: rest -> collect (value :: acc) rest
          | _ -> Error (Error.make Error.Usage "ignore_globs must contain only strings")
        in
        let* ignore_globs = collect [] values in
        Ok { settings with ignore_globs = Some ignore_globs }
    | "service_wait_s", Value.Float value ->
        Ok { settings with service_wait_s = Some value }
    | "service_wait_s", Value.Int value ->
        Ok { settings with service_wait_s = Some (float_of_int value) }
    | "verify_ssl", Value.Bool value -> Ok { settings with verify_ssl = Some value }
    | _ ->
        Error
          (Error.make Error.Usage
             (Printf.sprintf "unknown or invalid W&B setting %S" key))
  in
  List.fold_left
    (fun acc item ->
      let* settings = acc in
      update_one settings item)
    (Ok settings)
    values
