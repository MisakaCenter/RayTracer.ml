open Base
open Core
open Vec
open Ray
open Hittable
open Utils
open Float
open Material

class sphere cen r (mat_ptr: material pointer) = object 
inherit hittable

val mutable center: vec3 = cen
val mutable radius: float = r
val mutable mat_ptr = mat_ptr

method hit (r: ray)(t_min : float)(t_max : float) (rcd: hit_record pointer) =
  let oc = r#origin -| center in
  let a = r#direction#length_squared in
  let half_b = dot oc r#direction in
  let c = oc#length_squared -. (radius *. radius) in
  let discriminant = (half_b *. half_b) -. ( a *. c) in
  if  discriminant <. 0.0 then false
  else
    let sqrtd = sqrt discriminant in
    let root = ((-. half_b)  -. sqrtd) /. a in
    if ( (root <=. t_min) || (root >=. t_max) ) then (
      let root = ((-. half_b)  +. sqrtd) /. a in
      if ( (root <. t_min) ||  (root >=. t_max)) then false else begin
        let p_ = at r root in
          let outward_normal_ = (p_ -| center) /= radius in
            (rcd ^:= (set_face_normal { p = p_; t = root; normal = outward_normal_; front_face = (!^ rcd).front_face; mat_ptr = (!^ rcd).mat_ptr} r outward_normal_));
          true
      end)
    else begin
      let p_ = at r root in
          let outward_normal_ = (p_ -| center) /= radius in
          (rcd ^:= (set_face_normal { p = p_; t = root; normal = outward_normal_; front_face = (!^ rcd).front_face; mat_ptr = (!^ rcd).mat_ptr} r outward_normal_));
        true
    end
    

end