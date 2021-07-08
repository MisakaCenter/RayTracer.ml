open Base
open Vec
open Ray
open Core
open Float
open Aabb
open Utils
open Texture
open Array

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

class translate (p: hittable pointer) (displacement: vec3) =
  object
    inherit hittable
    val mutable ptr = p
    val mutable offset = displacement
    method hit (r : ray) (t_min : float) (t_max : float)
    (rcd : hit_record pointer) =
      let moved_ray = new ray (r#origin -| offset) (r#direction) r#time in
        if (not ((!^ptr)#hit moved_ray t_min t_max rcd)) then false
        else 
          (rcd
          ^:= set_face_normal
                {
                  p = !^rcd.p +| offset;
                  t = !^rcd.t;
                  normal = !^rcd.normal;
                  front_face = !^rcd.front_face;
                  mat_ptr = !^rcd.mat_ptr;
                  u = !^rcd.u;
                  v = !^rcd.v
                }
                  moved_ray (!^rcd).normal;
                true)

    method bounding_box time0 time1 output_box = 
      if (not ((!^ptr)#bounding_box time0 time1 output_box)) then false
      else (
        output_box ^:= new aabb ((!^output_box)#min +| offset) ((!^output_box)#max +| offset);
        true
      )
  end

class rotate_y (p: hittable pointer) (angle: float) =
  object
    inherit hittable
    val mutable ptr = p
    val mutable sin_theta = 0.0
    val mutable cos_theta = 0.0
    val mutable hasbox = true
    val mutable bbox = new_pointer init_aabb

    initializer 
      let radians = degrees_to_radians angle in
        sin_theta <- sin radians;
        cos_theta <- cos radians;
        hasbox <- (!^p)#bounding_box 0.0 1.0 bbox;
        let min_vec = [|infinity;infinity;infinity|] in
        let max_vec = [|-infinity;-infinity;-infinity|] in
          for i = 0 to 2 do
            for j = 0 to 2 do
              for k = 0 to 2 do
                let x = ((float_of_int i) *. (!^bbox#max)#x) +. ((1.0 -. (float_of_int i)) *. (!^bbox#min)#x) in
                let y = ((float_of_int j) *. (!^bbox#max)#y) +. ((1.0 -. (float_of_int j)) *. (!^bbox#min)#y) in
                let z = ((float_of_int k) *. (!^bbox#max)#z) +. ((1.0 -. (float_of_int k)) *. (!^bbox#min)#z) in
                let newx = (cos_theta *. x) +. ( sin_theta *. z) in
                let newz = (- sin_theta *. x) +. (cos_theta *. z) in
                let tester = [|newx;y;newz|] in
                for c = 0 to 2 do
                  set min_vec c (min (get min_vec c) (get tester c));
                  set max_vec c (max (get max_vec c) (get tester c))
                done
              done
            done
          done;
          bbox <- new_pointer (new aabb (new vec3 min_vec) (new vec3 max_vec))

    method hit (r : ray) (t_min : float) (t_max : float)
    (rcd : hit_record pointer) =
      let xx_o = r#origin#x in
      let zz_o = r#origin#z in
      let xx_d = r#direction#x in
      let zz_d = r#direction#z in
      let origin = [|((cos_theta *. xx_o) -. (sin_theta *. zz_o));r#origin#y;((sin_theta *. xx_o) +. (cos_theta *. zz_o))|] in
      let direction = [|((cos_theta *. xx_d) -. (sin_theta *. zz_d));r#direction#y;((sin_theta *. xx_d) +. (cos_theta *. zz_d))|] in
      let moved_ray = new ray (new vec3 origin) (new vec3 direction) r#time in
        if (not ((!^ptr)#hit moved_ray t_min t_max rcd)) then false
        else 
          let xx_p = (!^rcd).p#x in
          let zz_p = (!^rcd).p#z in
          let xx_n = (!^rcd).normal#x in
          let zz_n = (!^rcd).normal#z in
          let p = [|((cos_theta *. xx_p) +. (sin_theta *. zz_p));(!^rcd).p#y;((-sin_theta *. xx_p) +. (cos_theta *. zz_p))|] in
          let normal = [|((cos_theta *. xx_n) +. (sin_theta *. zz_n));(!^rcd).normal#y;((-sin_theta *. xx_n) +. (cos_theta *. zz_n))|] in
          (rcd
          ^:= set_face_normal
                {
                  p = (new vec3 p);
                  t = !^rcd.t;
                  normal = !^rcd.normal;
                  front_face = !^rcd.front_face;
                  mat_ptr = !^rcd.mat_ptr;
                  u = !^rcd.u;
                  v = !^rcd.v
                }
                  moved_ray (new vec3 normal);
                true)

    method bounding_box _ _ output_box = 
        output_box ^:= !^bbox;
        hasbox
  end

let init_material = new lambertian (new_pointer (new solid_color (new vec3 [| 0.0; 0.0; 0.0 |])))
