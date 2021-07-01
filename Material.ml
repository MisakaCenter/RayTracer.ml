
open Ray
open Vec
open Utils

type hit_record_safe = { p : vec3; normal : vec3; t : float; front_face : bool}

class virtual material (a : int)= object
  val mutable mat_type = a 
  method virtual scatter: ray -> hit_record_safe -> vec3 pointer -> ray pointer -> bool
end

class lambertia a mat_type= object
inherit material mat_type
val mutable albedo = a
method scatter _ rcd attenuation scattered = 
  let scatter_direction = new_pointer (rcd.normal +| random_unit_vector) in
  if !^ scatter_direction#near_zero then scatter_direction ^:= rcd.normal; 
  scattered ^:= new ray rcd.p (!^ scatter_direction);
  attenuation ^:= albedo;
  true
end
