type timestamp = int

type t = {
  level : Level.t;
  timestamp : timestamp;
  message : string;
}

val parse : string -> t option

val to_string : t -> string

val info : timestamp -> string -> t

val warn : timestamp -> string -> t

val error : int -> timestamp -> string -> t
