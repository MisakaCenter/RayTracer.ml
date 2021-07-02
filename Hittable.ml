open Base
open Vec
open Ray
open Core
open Float
open Utils

class virtual ['a] material_meta =
  object
    method virtual scatter : ray -> 'a -> vec3 pointer -> ray pointer -> bool
  end

type hit_record = {
  p : vec3;
  normal : vec3;
  t : float;
  front_face : bool;
  mat_ptr : hit_record material_meta pointer;
}

let set_face_normal h (r : ray) (outward_normal : vec3) =
  let ff = dot r#direction outward_normal <. 0.0 in
  let n = if ff then outward_normal else rev outward_normal in
  { p = h.p; normal = n; t = h.t; front_face = ff; mat_ptr = h.mat_ptr }

class virtual hittable =
  object
    method virtual hit : ray -> float -> float -> hit_record pointer -> bool
  end

class virtual material =
  object
    inherit [hit_record] material_meta

    method virtual scatter
        : ray -> hit_record -> vec3 pointer -> ray pointer -> bool
  end

class lambertian a =
  object
    val mutable albedo : vec3 = a

    method scatter (_ : ray) (rcd : hit_record) (attenuation : vec3 pointer)
        (scattered : ray pointer) =
      let scatter_direction =
        new_pointer (rcd.normal +| unit_vector (random_in_unit_sphere 1))
      in
      if !^scatter_direction#near_zero then scatter_direction ^:= rcd.normal;
      scattered ^:= new ray rcd.p !^scatter_direction;
      attenuation ^:= albedo;
      true
  end

class metal a b =
  object
    val mutable albedo : vec3 = a

    val mutable fuzz : float = b

    method scatter (r_in : ray) (rcd : hit_record) (attenuation : vec3 pointer)
        (scattered : ray pointer) =
      let reflected = reflect (unit_vector r_in#direction) rcd.normal in
      scattered
      ^:= new ray
            rcd.p
            (reflected +| (unit_vector (random_in_unit_sphere 1) *= fuzz));
      attenuation ^:= albedo;
      dot !^scattered#direction rcd.normal >. 0.0
  end
