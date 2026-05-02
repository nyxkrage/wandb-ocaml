type t = (string * Value.t) list

let empty = []
let of_list values = values
let get config key = List.assoc_opt key config
let set config key value = (key, value) :: List.remove_assoc key config
let update config values = List.fold_left (fun acc (k, v) -> set acc k v) config values
let keys config = List.map fst config
let bindings config = config

let of_json _json =
  Util.todo "Config.of_json"

let of_json_file _path =
  Util.todo "Config.of_json_file"
