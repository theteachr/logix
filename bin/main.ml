open Logix

let log_lines =
  [
    "I 147 mice in the air, I’m afraid, but you might catch a bat, and";
    "E 2 148 #56k istereadeat lo d200ff] BOOTMEM";
  ]

let () =
  log_lines
  |> List.filter_map Log.parse
  |> List.iter (fun message -> print_endline (Log.to_string message))
