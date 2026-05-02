let ( let* ) = Result.bind

let expand_user path =
  if path = "~" then
    Option.value (Sys.getenv_opt "HOME") ~default:path
  else if String.length path >= 2 && String.sub path 0 2 = "~/" then
    match Sys.getenv_opt "HOME" with
    | Some home -> Filename.concat home (String.sub path 2 (String.length path - 2))
    | None -> path
  else
    path

let url_netloc url =
  let after_scheme =
    if Util.starts_with ~prefix:"https://" url then
      Some (String.sub url 8 (String.length url - 8))
    else if Util.starts_with ~prefix:"http://" url then
      Some (String.sub url 7 (String.length url - 7))
    else
      None
  in
  match after_scheme with
  | None -> None
  | Some rest ->
      let stop =
        let len = String.length rest in
        let rec loop index =
          if index >= len then
            len
          else
            match rest.[index] with
            | '/' | '?' | '#' -> index
            | _ -> loop (index + 1)
        in
        loop 0
      in
      let netloc = String.sub rest 0 stop in
      if netloc = "" then None else Some netloc

let read_file path =
  try
    let channel = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let length = in_channel_length channel in
        Ok (really_input_string channel length))
  with
  | Sys_error message -> Error (Error.make Error.Io message)

let validate_api_key key =
  let key = String.trim key in
  let jwt_like =
    match String.split_on_char '.' key with
    | [ header; payload; signature ] ->
        header <> "" && payload <> "" && signature <> ""
    | _ -> false
  in
  if key = "" then
    Error (Error.make Error.Authentication "API key is empty.")
  else if jwt_like then
    Ok key
  else
    let secret =
      match String.split_on_char '-' key with
      | [] -> key
      | [ only ] -> only
      | _prefix :: rest -> String.concat "-" rest
    in
    let valid_char ch =
      ('a' <= ch && ch <= 'z')
      || ('A' <= ch && ch <= 'Z')
      || ('0' <= ch && ch <= '9')
      || ch = '_'
      || ch = '-'
    in
    if not (String.for_all valid_char secret) then
      Error
        (Error.make Error.Authentication
           "API key may only contain the letters A-Z, digits and underscores.")
    else if String.length secret < 40 then
      Error
        (Error.make Error.Authentication
           (Printf.sprintf "API key must have 40+ characters, has %d."
              (String.length secret)))
    else
      Ok key

let netrc_path () =
  match Sys.getenv_opt "NETRC" with
  | Some path -> expand_user path
  | None ->
      let unix_path = expand_user "~/.netrc" in
      let windows_path = expand_user "~/_netrc" in
      if Sys.file_exists unix_path then unix_path
      else if Sys.file_exists windows_path then windows_path
      else if Sys.os_type = "Win32" then windows_path
      else unix_path

let auth_from_netrc ~base_url =
  match url_netloc base_url with
  | None -> Ok None
  | Some machine ->
      let path = netrc_path () in
      if not (Sys.file_exists path) then
        Ok None
      else
        match read_file path with
        | Error _ -> Ok None
        | Ok contents ->
            let tokens =
              contents
              |> String.split_on_char '\n'
              |> List.map (fun line ->
                match String.split_on_char '#' line with
                | before_comment :: _ -> before_comment
                | [] -> line)
              |> String.concat " "
              |> String.split_on_char ' '
              |> List.map String.trim
              |> List.filter (fun token -> token <> "")
            in
            let rec password_in_machine = function
              | [] -> None
              | "machine" :: _ -> None
              | "password" :: password :: _ -> Some password
              | _ :: rest -> password_in_machine rest
            in
            let rec find_machine = function
              | [] -> None
              | "machine" :: name :: rest when name = machine -> password_in_machine rest
              | _ :: rest -> find_machine rest
            in
            match find_machine tokens with
            | None -> Ok None
            | Some key ->
                let* key = validate_api_key key in
                Ok (Some key)

let session_auth : (string * string) option ref = ref None

let authenticate ~base_url ~mode ~force:_ ~explicit_api_key =
  match mode with
  | Types.Mode.Offline | Types.Mode.Disabled -> Ok None
  | Types.Mode.Online | Types.Mode.Shared ->
      let use_key key =
        let* key = validate_api_key key in
        session_auth := Some (base_url, key);
        Ok (Some key)
      in
      match explicit_api_key with
      | Some key -> use_key key
      | None ->
          begin match !session_auth with
          | Some (host, key) when host = base_url -> Ok (Some key)
          | _ ->
              match Sys.getenv_opt "WANDB_API_KEY", Sys.getenv_opt "WANDB_IDENTITY_TOKEN_FILE" with
              | Some _, Some _ ->
                  Error
                    (Error.make Error.Authentication
                       "Both WANDB_API_KEY and WANDB_IDENTITY_TOKEN_FILE are set, which is not allowed.")
              | Some key, None -> use_key key
              | None, Some path ->
                  session_auth := Some (base_url, "identity-token-file:" ^ path);
                  Ok None
              | None, None ->
                  let* netrc_key = auth_from_netrc ~base_url in
                  match netrc_key with
                  | Some key -> use_key key
                  | None ->
                      Error
                        (Error.make Error.Usage
                           "No API key configured. Use `wandb login` to log in.")
          end
