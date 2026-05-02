let todo name =
  failwith ("TODO(wandb-ocaml): implement " ^ name)

let default_base_url = "https://api.wandb.ai"

let opt_first left right =
  match right with
  | Some _ -> right
  | None -> left

let split_comma value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun value -> value <> "")

let getenv ?environ name =
  match environ with
  | Some values -> List.assoc_opt name values
  | None -> Sys.getenv_opt name

let starts_with ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.sub value 0 prefix_len = prefix

let rec value_to_yojson = function
  | Value.Null -> `Null
  | Value.Bool value -> `Bool value
  | Value.Int value -> `Int value
  | Value.Int64 value -> `Intlit (Int64.to_string value)
  | Value.Float value -> `Float value
  | Value.String value -> `String value
  | Value.List values -> `List (List.map value_to_yojson values)
  | Value.Assoc values ->
      `Assoc (List.map (fun (key, value) -> key, value_to_yojson value) values)
