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

class moving_sphere (cen0 : vec3) (cen1 : vec3) (_time0 : float) (_time1 : float) (r : float) (m: hit_record material_meta pointer)=
  object
    inherit hittable

    val mutable center0 : vec3 = cen0

    val mutable center1 : vec3 = cen1

    val mutable time0 = _time0

    val mutable time1 = _time1

    val mutable radius : float = r

    val mutable mat_ptr = m
    
    method hit (r : ray) (t_min : float) (t_max : float)
        (rcd : hit_record pointer) =
      let center = center0 +| ((center1 -| center0) *= ((r#time -. time0) /. (time1 -. time0))) in
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
