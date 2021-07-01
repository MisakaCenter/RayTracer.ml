open Core
open Hittable
open Array
open Ray
open Vec
open Utils
open Material

class hittable_list (init: hittable array) = object
  inherit hittable
  val mutable objects = init

  method add obj: hittable_list = 
    new hittable_list (append [|obj|] objects)
  
   method hit (r: ray)(t_min : float)(t_max : float) (rcd: hit_record pointer) = 
      let temp_record = new_pointer {p=new vec3 [||]; normal = new vec3 [||]; t = 0.0; front_face = false; mat_ptr = new_pointer (new lambertia (new vec3 [|0.8;0.8;0.0|]) 1)} in
      let hit_anything = new_pointer false in
      let closest_so_far = new_pointer t_max in (
        for i = 0 to ((length objects) - 1) do
          let obj = objects.(i) in
            if (obj#hit r t_min (!^ closest_so_far) temp_record) then begin
              hit_anything ^:= true;
              closest_so_far ^:= (!^ temp_record).t;
              rcd ^:= (!^ temp_record);
            end
        done;
        !^ hit_anything)
        
end