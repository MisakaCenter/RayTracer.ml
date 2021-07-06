open Core
open Float
let pi = 3.1415926535897932385

let infinity = infinity

let degrees_to_radians degrees = degrees *. pi /. 180.0

type 'a pointer = Null | Pointer of 'a ref

let ( !^ ) = function
  | Null -> invalid_arg "Attempt to dereference the null pointer"
  | Pointer r -> !r

let ( ^:= ) p v =
  match p with
  | Null -> invalid_arg "Attempt to assign the null pointer"
  | Pointer r -> r := v

let new_pointer x = Pointer (ref x)

let random_float n = Random.float n

let random_float_n_m n m = n +. ((m -. n) *. (random_float 1.0))

let random_int_n_m n m = Random.int_incl n m
let clamp (x : float) min max : float =
  if x <. min then min else if x >. max then max else x
