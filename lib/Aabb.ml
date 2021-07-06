open Utils
open Vec
open Ray
open Array
open Core
open Float

class aabb (a : vec3) (b : vec3) =
object
  val mutable minimum = a
  val mutable maximum = b
  method min = minimum
  method max = maximum

  method hit (r : ray) (t_min : float) (t_max : float): bool =
    let result = new_pointer true in
    for a = 0 to 2 do 
      let t0 = Float.min_inan ((get minimum#l a -. get r#origin#l a) /. get r#direction#l a) ((get maximum#l a -. get r#origin#l a) /. get r#direction#l a) in
      let t1 = Float.max_inan ((get minimum#l a -. get r#origin#l a) /. get r#direction#l a) ((get maximum#l a -. get r#origin#l a) /. get r#direction#l a) in
      let t_min = Float.max_inan t0 t_min in
      let t_max = Float.min_inan t1 t_max in
      if t_max <. t_min then result ^:= false;
    done;
    !^result
end

let surrounding_box box0 box1 : aabb = 
  let small = new vec3 [|Float.min_inan (box0#min#x) (box1#min#x);
                          Float.min_inan (box0#min#y) (box1#min#y);
                          Float.min_inan (box0#min#z) (box1#min#z)|] in
  let big = new vec3 [|Float.max_inan (box0#min#x) (box1#min#x);
                          Float.max_inan (box0#min#y) (box1#min#y);
                          Float.max_inan (box0#min#z) (box1#min#z)|] in
  new aabb small big

let init_aabb = (new aabb (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|0.0;0.0;0.0|]))