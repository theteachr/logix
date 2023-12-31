open Logix

let lines =
  [
    "I 6 Completed armadillo processing";
    "I 1 Nothing to report";
    "E 99 10 Flange failed!";
    "I 4 Everything normal";
    "I 11 Initiating self-destruct sequence";
    "E 70 3 Way too many pickles";
    "E 65 8 Bad pickle-flange interaction detected";
    "W 5 Flange is due for a check-up";
    "I 7 Out for lunch, back in two time steps";
    "E 20 2 Too many pickles";
    "I 9 Back from lunch";
  ]

let what_went_wrong log_messages =
  let relevant_message Message.{ level; message; _ } =
    match level with
    | Error severity when severity > 50 -> Some message
    | _ -> None
  in
  log_messages
  |> List.filter_map Message.parse
  |> Message_tree.build
  |> Message_tree.in_order
  |> List.filter_map relevant_message

let () = lines |> what_went_wrong |> List.iter print_endline
