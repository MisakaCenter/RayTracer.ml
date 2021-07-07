open Base
open Vec
open Ray
open Core
open Float
open Aabb
open Utils
open Texture

class virtual ['a] material_meta =
  object
    method virtual scatter : ray -> 'a -> vec3 pointer -> ray pointer -> bool
    method virtual emitted :float->float->vec3->vec3
  end

type hit_record = {
  p : vec3;
  normal : vec3;
  t : float;
  front_face : bool;
  mat_ptr : hit_record material_meta pointer;
  u: float;
  v: float
}

let set_face_normal h (r : ray) (outward_normal : vec3) =
  let ff = dot r#direction outward_normal <. 0.0 in
  let n = if ff then outward_normal else rev outward_normal in
  { p = h.p; normal = n; t = h.t; front_face = ff; mat_ptr = h.mat_ptr; u = h.u; v = h.v }

class virtual hittable =
  object
    method virtual hit : ray -> float -> float -> hit_record pointer -> bool
    method virtual bounding_box : float -> float -> aabb pointer -> bool
  end

class virtual material =
  object
    inherit [hit_record] material_meta

    method virtual scatter
        : ray -> hit_record -> vec3 pointer -> ray pointer -> bool
    method virtual emitted :float->float->vec3->vec3
  end

class lambertian a =
  object
    inherit material
    val mutable albedo : texture pointer = a

    method scatter (r_in : ray) (rcd : hit_record) (attenuation : vec3 pointer)
        (scattered : ray pointer) =
      let scatter_direction =
        new_pointer (rcd.normal +| unit_vector (random_in_unit_sphere 1))
      in
      if !^scatter_direction#near_zero then scatter_direction ^:= rcd.normal;
      scattered ^:= new ray rcd.p !^scatter_direction (r_in#time);
      attenuation ^:= (!^albedo)#value rcd.u rcd.v rcd.p;
      true
    method emitted (_:float)(_:float)(_:vec3):vec3 = new vec3 [|0.0;0.0;0.0|]
  end

class diffuse_light a =
  object
    inherit material
    val mutable emit : texture pointer = a

    method scatter (_ : ray) (_ : hit_record) (_ : vec3 pointer)
        (_ : ray pointer) =
      false
    method emitted (u:float)(v:float)(p:vec3):vec3 = 
      (!^ emit)#value u v p
  end

class metal a b =
  object
    inherit material
    val mutable albedo : vec3 = a

    val mutable fuzz : float = b

    method scatter (r_in : ray) (rcd : hit_record) (attenuation : vec3 pointer)
        (scattered : ray pointer) =
      let reflected = reflect (unit_vector r_in#direction) rcd.normal in
      scattered
      ^:= new ray
            rcd.p
            (reflected +| (unit_vector (random_in_unit_sphere 1) *= fuzz))(r_in#time);
      attenuation ^:= albedo;
      dot !^scattered#direction rcd.normal >. 0.0
    method emitted (_:float)(_:float)(_:vec3):vec3 = new vec3 [|0.0;0.0;0.0|]
  end

(* Use Schlick's approximation for reflectance. *)
let reflectance cosine ref_idx = 
  let r0 = (1.0 -. ref_idx) /. (1.0 +. ref_idx) in
  let r0 = r0 *. r0 in
  r0 +. ((1.0 -. r0) *. ((1.0 -. cosine) ** 5.0))

class dielectric a =
  object
    inherit material
    val mutable ir : float = a

    method scatter (r_in : ray) (rcd : hit_record) (attenuation : vec3 pointer)
        (scattered : ray pointer) =
      attenuation ^:=  new vec3 [| 1.0; 1.0; 1.0 |];
      let refraction_ratio = if rcd.front_face then 1.0 /. ir else ir in
      let unit_direction = unit_vector r_in#direction in

      let cos_theta = min (dot (rev unit_direction) rcd.normal) 1.0 in
      let sin_theta = sqrt (1.0 -. (cos_theta *. cos_theta)) in
      let cannot_refract = refraction_ratio *. sin_theta >. 1.0 in
      let direction = if cannot_refract || (reflectance cos_theta refraction_ratio >. random_float 1.0) then reflect unit_direction rcd.normal else refract unit_direction rcd.normal refraction_ratio in
      scattered ^:= new ray rcd.p direction (r_in#time);
      true
    method emitted (_:float)(_:float)(_:vec3):vec3 = new vec3 [|0.0;0.0;0.0|]
  end


let init_material = new lambertian (new_pointer (new solid_color (new vec3 [| 0.0; 0.0; 0.0 |])))
