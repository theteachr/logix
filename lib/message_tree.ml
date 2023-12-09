(* TODO: Create a generic BST *)
type t =
  | Leaf
  | Node of {
      left : t;
      entry : Message.t;
      right : t;
    }

(* TCO might not be worth it *)

let rec insert Message.({ timestamp; _ } as entry) = function
  | Leaf -> Node { left = Leaf; entry; right = Leaf }
  | Node node when timestamp <= node.entry.timestamp ->
      Node { node with left = insert entry node.left }
  | Node node -> Node { node with right = insert entry node.right }

let build = List.fold_left (Fun.flip insert) Leaf

let rec in_order = function
  | Leaf -> []
  | Node { left = Leaf; entry; right } -> entry :: in_order right
  | Node { left; entry; right } -> in_order left @ (entry :: in_order right)
