let ( let* ) = Option.bind

type timestamp = int

type t = {
  message : string;
  level : Log_level.t;
  timestamp : timestamp;
}

let _make ~message ~level ~timestamp = { message; level; timestamp }

let parse line =
  let use_line line = Scanf.sscanf line in
  let difficult _ line =
      use_line line "%_c %d %d %[^\n]" (fun severity timestamp message ->
          { level = Log_level.Error severity; timestamp; message })
      |> Option.some
  in
  let simple level line =
    let* level = level in
    Scanf.sscanf line "%_c %d %[^\n]" (fun timestamp message ->
        { level; timestamp; message })
    |> Option.some
  in
  let level, f =
    match String.get line 0 with
    | 'I' -> (Some Log_level.Info, simple)
    | 'W' -> (Some Log_level.Warn, simple)
    | 'E' -> (None, difficult)
    | _ -> (None, fun _ _ -> None)
  in
  f level line

let to_string { timestamp; level; message } =
  Printf.sprintf "[%d] [%s] %s" timestamp (Log_level.to_string level) message
