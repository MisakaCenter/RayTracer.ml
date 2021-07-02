open Base
open Vec
open Ray
open Hittable
open Utils
open Core
open Float

class sphere cen r (m : hit_record material_meta pointer) =
  object
    inherit hittable

    val mutable center : vec3 = cen

    val mutable radius : float = r

    val mutable mat_ptr = m

    method hit (r : ray) (t_min : float) (t_max : float)
        (rcd : hit_record pointer) =
      let oc = r#origin -| center in
      let a = r#direction#length_squared in
      let half_b = dot oc r#direction in
      let c = oc#length_squared -. (radius *. radius) in
      let discriminant = (half_b *. half_b) -. (a *. c) in
      if discriminant <. 0.0 then false
      else
        let sqrtd = sqrt discriminant in
        let root = (0.0 -. half_b -. sqrtd) /. a in
        if root <. t_min || root >. t_max then (
          let root = (0.0 -. half_b +. sqrtd) /. a in
          if root <. t_min || root >. t_max then
            false
          else
            let p = at r root in
            let outward_normal = (p -| center) /= radius in
            rcd
            ^:= set_face_normal
                  {
                    p;
                    t = root;
                    normal = (p -| center) /= radius;
                    front_face = true;
                    mat_ptr;
                  }
                  r outward_normal;
            true)
        else
          let p = at r root in
          let outward_normal = (p -| center) /= radius in
          rcd
          ^:= set_face_normal
                {
                  p;
                  t = root;
                  normal = (p -| center) /= radius;
                  front_face = true;
                  mat_ptr;
                }
                r outward_normal;
          true
  end
