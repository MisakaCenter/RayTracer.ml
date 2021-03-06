open Array
open Utils
open Core
open Float

class vec3 (init : float array) =
  object
    val mutable v = init

    method x = get v 0

    method y = get v 1

    method z = get v 2

    method l = v

    method length_squared =
      (get v 0 *. get v 0) +. (get v 1 *. get v 1) +. (get v 2 *. get v 2)

    method length =
      sqrt ((get v 0 *. get v 0) +. (get v 1 *. get v 1) +. (get v 2 *. get v 2))

    method near_zero =
      let s = 0.0000001 in
      abs (get v 0) <. s && abs (get v 1) <. s && abs (get v 2) <. s
  end

let rev (x : vec3) : vec3 = new vec3 (map (fun x -> 0.0 -. x) x#l)

let ( +| ) (a : vec3) (b : vec3) =
  new vec3 [| a#x +. b#x; a#y +. b#y; a#z +. b#z |]

let ( -| ) (a : vec3) (b : vec3) =
  new vec3 [| a#x -. b#x; a#y -. b#y; a#z -. b#z |]

let ( *| ) (a : vec3) (b : vec3) =
  new vec3 [| a#x *. b#x; a#y *. b#y; a#z *. b#z |]

let ( /| ) (a : vec3) (b : vec3) =
  new vec3 [| a#x /. b#x; a#y /. b#y; a#z /. b#z |]

let ( += ) (a : vec3) (b : float) = new vec3 [| a#x +. b; a#y +. b; a#z +. b |]

let ( -= ) (a : vec3) (b : float) = new vec3 [| a#x -. b; a#y -. b; a#z -. b |]

let ( *= ) (a : vec3) (b : float) = new vec3 [| a#x *. b; a#y *. b; a#z *. b |]

let ( /= ) (a : vec3) (b : float) = new vec3 [| a#x /. b; a#y /. b; a#z /. b |]

let cross (a : vec3) (b : vec3) =
  new vec3
    [|
      (a#y *. b#z) -. (a#z *. b#y);
      (a#z *. b#x) -. (a#x *. b#z);
      (a#x *. b#y) -. (a#y *. b#x);
    |]

let dot (a : vec3) (b : vec3) = (a#x *. b#x) +. (a#y *. b#y) +. (a#z *. b#z)

let unit_vector (a : vec3) =
  new vec3 [| a#x /. a#length; a#y /. a#length; a#z /. a#length |]

let to_string (v : vec3) =
  Int.to_string (int_of_float v#x)
  ^ " "
  ^ Int.to_string (int_of_float v#y)
  ^ " "
  ^ Int.to_string (int_of_float v#z)

let random_vec_n_m n m =
  new vec3
    [| random_float_n_m n m; random_float_n_m n m; random_float_n_m n m |]

let rec random_in_unit_sphere (aa : int) =
  let p = random_vec_n_m (-1.0) 1.0 in
  if p#length_squared >. 1.0 then random_in_unit_sphere aa else p

let rec random_in_unit_disk (aa : int) =
  let p = new vec3 [|random_float_n_m (-1.0) 1.0; random_float_n_m (-1.0) 1.0; 0.0|] in
  if p#length_squared >. 1.0 then random_in_unit_disk aa else p

(* For metal *)
let reflect (v : vec3) n = v -| (n *= (dot v n *. 2.0))

(* For glass *)
let refract (uv: vec3) (n: vec3) (etai_over_etat: float): vec3 =
  let cos_theta = min (dot (rev uv) n) 1.0 in
  let r_out_perp = (uv +| (n *= cos_theta) ) *= etai_over_etat in
  let r_out_parallel = n *= (- sqrt (abs (1.0 -. r_out_perp#length_squared))) in
  r_out_perp +| r_out_parallel
