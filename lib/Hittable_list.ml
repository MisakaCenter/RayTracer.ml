open Core
open Hittable
open Array
open Ray
open Vec
open Utils
open Aabb
open Texture

class hittable_list (init : hittable array) =
  object
    inherit hittable

    val mutable objects = init

    method objs = objects

    method add obj : hittable_list =
      new hittable_list (append [| obj |] objects)
    
    method bounding_box time0 time1 output_box: bool = 
      if (length objects = 0) then false
      else 
        let first_box = new_pointer true in
        let res = new_pointer true in
        let temp_box = new_pointer init_aabb in
        let max_iter = length objects - 1 in 
          for i = 0 to max_iter do
            let obj = objects.(i) in
              if (obj#bounding_box time0 time1 temp_box) then 
                (res ^:= false;)
              else 
                (if !^ res then
                  (output_box ^:= if !^ first_box then !^ temp_box else surrounding_box !^ output_box !^ temp_box;
                  first_box ^:= false;))
          done;
        !^ res
    
    method hit (r : ray) (t_min : float) (t_max : float)
        (rcd : hit_record pointer) =
      let temp_record =
        new_pointer
          {
            p = new vec3 [||];
            normal = new vec3 [||];
            t = 0.0;
            front_face = false;
            mat_ptr = new_pointer (new lambertian (new_pointer (new solid_color (new vec3 [| 0.0; 0.0; 0.0 |]))));
            u = 0.0;
            v = 0.0
          }
      in
      let hit_anything = new_pointer false in
      let closest_so_far = new_pointer t_max in
      let max_iter = length objects - 1 in 
      for i = 0 to max_iter do
        let obj = objects.(i) in
        if obj#hit r t_min !^closest_so_far temp_record then (
          hit_anything ^:= true;
          closest_so_far ^:= !^temp_record.t;
          rcd ^:= !^temp_record)
      done;
      !^hit_anything
  end
