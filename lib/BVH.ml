open Hittable
open Hittable_list
open Utils
open Vec
open Aabb
open Array
open Core
open Sphere

let box_compare (a:hittable) (b:hittable) (axis:int) = 
  let box_a = new_pointer (new aabb (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|0.0;0.0;0.0|])) in
  let box_b = new_pointer (new aabb (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|0.0;0.0;0.0|])) in
  if (not (a#bounding_box 0.0 0.0 box_a) || not (b#bounding_box 0.0 0.0 box_b)) then Printf.printf "No bounding box in bvh_node constructor.\n";
  (Float.(<.) (get (!^ box_a)#min#l axis) (get (!^ box_b)#min#l axis))

let box_x_compare a b = box_compare a b 0

let box_y_compare a b = box_compare a b 1

let box_z_compare a b = box_compare a b 2

class bvh_node (objects : hittable array pointer) (start : int) (end_when : int) (time0 :float) (time1 : float)  =
object (self)
  inherit hittable
  val mutable left = init_sphere

  val mutable right = init_sphere
    
  val mutable box = init_aabb

  initializer left <- self#bvh_node_left objects start end_when time0 time1;
              right <- self#bvh_node_right objects start end_when time0 time1;
              let box_left = new_pointer (new aabb (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|0.0;0.0;0.0|])) in
              let box_right = new_pointer (new aabb (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|0.0;0.0;0.0|])) in
              if (not (left#bounding_box time0 time1 box_left) || not (right#bounding_box time0 time1 box_right)) then (Printf.printf "No bounding box in bvh_node constructor.\n";);
              box <- (surrounding_box (!^ box_left) (!^ box_right))

  method hit r t_min t_max rcd =
    if (not (box#hit r t_min t_max)) then false else
      let hit_left = left#hit r t_min t_max rcd in
      let hit_right = if hit_left then (right#hit r t_min (!^ rcd).t rcd) else (right#hit r t_min t_max rcd) in
      hit_left || hit_right
  
  method bounding_box _ _ output_box =
    output_box ^:= box;
    true
  
  method private bvh_node_left (src_objects : hittable array pointer) (start : int) (end_when : int) (time0 :float) (time1 : float) =
    let objects = src_objects in
    let axis = random_int_n_m  0 2 in
    let comparator = (if (axis = 0) then box_x_compare else if (axis = 1) then box_y_compare else box_z_compare) in
    let object_span = end_when - start in
    let result = new_pointer init_sphere in 
    if (object_span = 1) then
      (result ^:= get (!^ objects) start;)
    else if (object_span = 2) then
        (
          if (comparator (get (!^ objects) start) (get (!^ objects) (start + 1))) then
            (result ^:= get (!^ objects) start;)
          else
            result ^:= get (!^ objects) (start + 1);
        ) else (
          sort (fun x y -> if comparator x y then 1 else -1) (!^ objects);
          let mid = start + object_span/2 in
          result ^:= new bvh_node objects start mid time0 time1
        );
    !^ result

  method private bvh_node_right (src_objects : hittable array pointer) (start : int) (end_when : int) (time0 :float) (time1 : float) =
  let objects = src_objects in
  let axis = random_int_n_m  0 2 in
  let comparator = (if (axis = 0) then box_x_compare else if (axis = 1) then box_y_compare else box_z_compare) in
  let object_span = end_when - start in
  let result = new_pointer init_sphere in 
  if (object_span = 1) then
    (result ^:= get (!^ objects) start;)
  else if (object_span = 2) then
      (
        if (comparator (get (!^ objects) start) (get (!^ objects) (start + 1))) then
          (result ^:= get (!^ objects) (start + 1);)
        else
          result ^:= get (!^ objects) start;
      ) else (
        sort (fun x y -> if comparator x y then 1 else -1) (!^ objects);
        let mid = start + object_span/2 in
        result ^:= new bvh_node objects mid end_when time0 time1
      );
  !^ result
end

(* let bvh_node_left (src_objects : hittable array pointer) (start : int) (end_when : int) (time0 :float) (time1 : float) =
  let objects = (!^ src_objects) in
  let axis = random_int_n_m  0 2 in
  let comparator = (if (axis = 0) then box_x_compare else if (axis = 1) then box_y_compare else box_z_compare) in
  let object_span = end_when - start in
  let result = new_pointer init_sphere in 
  if (object_span = 1) then
    (result ^:= get objects start;)
  else if (object_span = 2) then
      (
        if (comparator (get objects start) (get objects (start + 1))) then
          (result ^:= get objects start;)
        else
          result ^:= get objects (start + 1);
      ) else (
        sort (fun x y -> if comparator x y then 1 else -1) objects;
        let mid = start + object_span/2 in
        result ^:= new bvh_node objects start mid time0 time1
      );
  !^ result *)

(* let bvh_node_right (src_objects : hittable array pointer) (start : int) (end_when : int) (time0 :float) (time1 : float) =
  let objects = (!^ src_objects) in
  let axis = random_int_n_m  0 2 in
  let comparator = (if (axis = 0) then box_x_compare else if (axis = 1) then box_y_compare else box_z_compare) in
  let object_span = end_when - start in
  let result = new_pointer init_sphere in 
  if (object_span = 1) then
    (result ^:= get objects start;)
  else if (object_span = 2) then
      (
        if (comparator (get objects start) (get objects (start + 1))) then
          (result ^:= get objects (start + 1);)
        else
          result ^:= get objects start;
      ) else (
        sort (fun x y -> if comparator x y then 1 else -1) objects;
        let mid = start + object_span/2 in
        result ^:= new bvh_node objects mid end_when time0 time1
      );
  !^ result *)

let new_bvh_node (list: hittable_list) (time0 : float) (time1 : float) =
  let objects = new_pointer (list#objs) in
    let start = 0 in
    let end_when = length (!^ objects) in
    new bvh_node objects start end_when time0 time1