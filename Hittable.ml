open Base
open Vec
open Ray
open Core
open Float
open Material
open Utils

type hit_record = { p : vec3; normal : vec3; t : float; front_face : bool; mat_ptr: material pointer}

let safe rcd = {
  p = rcd.p;
  normal = rcd.normal;
  t = rcd.t;
  front_face = rcd.front_face;
}

let set_face_normal h (r : ray) (outward_normal : vec3) =
  let ff = dot r#direction outward_normal <. 0.0 in
  let n = if ff then outward_normal else rev outward_normal in
  { p = h.p; normal = n; t = h.t; front_face = ff ; mat_ptr = h.mat_ptr}

class virtual hittable =
  object
    method virtual hit : ray -> float -> float -> hit_record pointer -> bool
  end
