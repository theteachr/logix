type timestamp = int

type t = {
  level : Log_level.t;
  timestamp : timestamp;
  message : string;
}

let make level timestamp message = { level; timestamp; message }

let info = make Info

let warn = make Warn

let error severity = make (Error severity)

let parse line =
  let read_log format receiver line =
    try Some (Scanf.sscanf line format receiver) with _ -> None
  in
  let scan =
    match String.get line 0 with
    | 'I' -> read_log "%_c %d %[^\n]" info
    | 'W' -> read_log "%_c %d %[^\n]" warn
    | 'E' -> read_log "%_c %d %d %[^\n]" error
    | _ -> fun _ -> None
  in
  scan line

let to_string { level; timestamp; message } =
  Printf.sprintf "[%d] [%s] %s" timestamp (Log_level.to_string level) message
