open Logix

let log_lines =
  [
    "I 147 mice in the air, I’m afraid, but you might catch a bat, and";
    "E 2 148 #56k istereadeat lo d200ff] BOOTMEM";
    "I 4 world";
    "I 0 BEGIN";
    "I 4 hello";
    "E 10 6 No stdout";
  ]

let () =
  log_lines
  |> List.filter_map Log.parse
  |> Message_tree.build
  |> Message_tree.in_order
  |> List.iter (fun message -> print_endline (Log.to_string message))
