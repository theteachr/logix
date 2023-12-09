(* TODO: Create a generic BST *)
type t =
  | Leaf
  | Node of {
      left : t;
      message : Message.t;
      right : t;
    }

(* TCO might not be worth it *)

let rec insert Message.({ timestamp; _ } as message) = function
  | Leaf -> Node { left = Leaf; message; right = Leaf }
  | Node node when timestamp <= node.message.timestamp ->
      Node { node with left = insert message node.left }
  | Node node -> Node { node with right = insert message node.right }

let build = List.fold_left (Fun.flip insert) Leaf

let rec in_order = function
  | Leaf -> []
  | Node { left = Leaf; message; right } -> message :: in_order right
  | Node { left; message; right } -> in_order left @ (message :: in_order right)
