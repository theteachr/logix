module Log_level = struct
  type severity = int

  type t =
    | Info
    | Warn
    | Error of severity

  let to_string = function
    | Info -> Printf.sprintf "INFO"
    | Warn -> Printf.sprintf "WARN"
    | Error sev -> Printf.sprintf "ERROR (%d)" sev
end

let ( let* ) = Option.bind

module Log = struct
  type timestamp = int

  type t = {
    message : string;
    level : Log_level.t;
    timestamp : timestamp;
  }

  let parse line =
    let* log_level, ts, rest =
      match String.split_on_char ' ' line with
      | "I" :: ts :: rest -> Some (Log_level.Info, ts, rest)
      | "W" :: ts :: rest -> Some (Log_level.Warn, ts, rest)
      | "E" :: sev :: ts :: rest ->
          let* sev = int_of_string_opt sev in
          Some (Log_level.Error sev, ts, rest)
      | _ -> None
    in
    let* timestamp = int_of_string_opt ts in
    Some { message = String.concat " " rest; level = log_level; timestamp }

  let to_string { timestamp; level; message } =
    Printf.sprintf "[%d] [%s] %s" timestamp (Log_level.to_string level) message
end

let log_lines =
  [
    "I 147 mice in the air, I’m afraid, but you might catch a bat, and";
    "E 2 148 #56k istereadeat lo d200ff] BOOTMEM";
  ]

let () =
  log_lines
  |> List.filter_map Log.parse
  |> List.iter (fun message -> print_endline (Log.to_string message))
