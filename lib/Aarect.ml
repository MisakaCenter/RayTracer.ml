open Base
open Vec
open Ray
open Hittable
open Utils
open Core
open Float
open Aabb

class xy_rect _x0 _x1 _y0 _y1 _k (m : hit_record material_meta pointer) =
  object
    inherit hittable

    val mutable x0:float = _x0
    val mutable x1:float = _x1
    val mutable y0:float = _y0
    val mutable y1:float = _y1
    val mutable k:float = _k
    val mutable mat_ptr = m

    method hit (r : ray) (t_min : float) (t_max : float)
        (rcd : hit_record pointer) =
        let t = (k -. r#origin#z) /. (r#direction#z) in
        if (t <. t_min || t >. t_max) then false else
          begin
            let x = r#origin#x +. (t *. r#direction#x) in
            let y = r#origin#y +. (t *. r#direction#y) in
            if (x <. x0 || x >. x1 || y <. y0 || y >. y1) then false else
              begin
                let outward_normal = new vec3 [|0.0;0.0;1.0|] in
                let u_ = (x -. x0) /. (x1 -. x0) in
                let v_ = (y -. y0) /. (y1 -. y0) in
                rcd
                ^:= set_face_normal
                      {
                        p = at r t ;
                        t = t;
                        normal = outward_normal;
                        front_face = true;
                        mat_ptr;
                        u = u_;
                        v =v_
                      }
                      r outward_normal;
                true
              end
          end

    method bounding_box (_ : float) (_ : float) output_box = 
        output_box ^:= new aabb 
        (new vec3 [|x0;y0;k -. 0.0001|]) (new vec3 [|x1;y1;k +. 0.0001|]);
        true

  end