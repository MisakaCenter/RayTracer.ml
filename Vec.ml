open Array

class vec3 (init : float array) =
  object
    val mutable v = init

    method x = get v 0

    method y = get v 1

    method z = get v 2

    method l = v

    method length_squared =
      (get v 0 *. get v 0) +. (get v 1 *. get v 1) +. (get v 1 *. get v 1)

    method length =
      sqrt ((get v 0 *. get v 0) +. (get v 1 *. get v 1) +. (get v 1 *. get v 1))
  end

let rev (x : vec3) : vec3 = new vec3 (map (fun x -> 0.0 -. x) x#l)

let add (a : vec3) (b : vec3) =
  new vec3 [| a#x +. b#x; a#y +. b#y; a#z +. b#z |]

let minus (a : vec3) (b : vec3) =
  new vec3 [| a#x -. b#x; a#y -. b#y; a#z -. b#z |]

let mul (a : vec3) (b : vec3) =
  new vec3 [| a#x *. b#x; a#y *. b#y; a#z *. b#z |]

let div (a : vec3) (b : vec3) =
  new vec3 [| a#x /. b#x; a#y /. b#y; a#z /. b#z |]

let addeq (a : vec3) (b : float) = new vec3 [| a#x +. b; a#y +. b; a#z +. b |]

let minuseq (a : vec3) (b : float) = new vec3 [| a#x -. b; a#y -. b; a#z -. b |]

let muleq (a : vec3) (b : float) = new vec3 [| a#x *. b; a#y *. b; a#z *. b |]

let diveq (a : vec3) (b : float) = new vec3 [| a#x /. b; a#y /. b; a#z /. b |]

let cross (a : vec3) (b : vec3) =
  new vec3
    [|
      (a#y *. b#z) -. (a#z *. b#y);
      (a#z *. b#x) -. (a#x *. b#z);
      (a#x *. b#y) -. (a#y *. b#x);
    |]

let unit_vector (a : vec3) =
  new vec3 [| a#x /. a#length; a#y /. a#length; a#z /. a#length |]

let to_string (v : vec3) =
  Int.to_string (int_of_float v#x)
  ^ " "
  ^ Int.to_string (int_of_float v#y)
  ^ " "
  ^ Int.to_string (int_of_float v#z)

(* external ( +~ ) : vec3 -> vec3 -> vec3 = "%add"
external ( -~ ) : vec3 -> vec3 -> vec3 = "%minus"
external ( *~ ) : vec3 -> vec3 -> vec3 = "%mul"
external ( /~ ) : vec3 -> vec3 -> vec3 = "%div"
external ( += ) : vec3 -> vec3 -> vec3 = "%addeq"
external ( -= ) : vec3 -> vec3 -> vec3 = "%minuseq"
external ( *= ) : vec3 -> vec3 -> vec3 = "%muleq"
external ( /= ) : vec3 -> vec3 -> vec3 = "%diveq"
external ( ~~ ) : vec3 -> vec3 = "%rev" *)
