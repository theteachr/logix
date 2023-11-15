type severity = int

type t =
  | Info
  | Warn
  | Error of severity

let to_string = function
  | Info -> Printf.sprintf "INFO"
  | Warn -> Printf.sprintf "WARN"
  | Error sev -> Printf.sprintf "ERROR (%d)" sev
