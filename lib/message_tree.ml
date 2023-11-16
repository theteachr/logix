type t =
  | Leaf
  | Node of {
      left : t;
      log : Log.t;
      right : t;
    }

let rec insert ({ Log.timestamp; _ } as log) = function
  | Leaf -> Node { left = Leaf; log; right = Leaf }
  | Node node when timestamp <= node.log.timestamp ->
      Node { node with left = insert log node.left }
  | Node node -> Node { node with right = insert log node.right }

let build = List.fold_left (Fun.flip insert) Leaf

let rec in_order = function
  | Leaf -> []
  | Node { left = Leaf; log; right } -> log :: in_order right
  | Node { left; log; right } -> in_order left @ (log :: in_order right)
