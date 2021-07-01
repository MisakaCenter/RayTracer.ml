open Random

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

let random_float n = float n

let random_float_n_m n m = n +. ((m -. n) *. random_float 1.0)

let clamp (x : float) min max : float =
  if compare x min < 0 then min else if compare x max > 0 then max else x
