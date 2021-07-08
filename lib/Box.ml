open Base
open Vec
open Ray
open Hittable
open Hittable_list
open Utils
open Aarect
open Aabb

class box (p0: vec3) (p1: vec3) (m : hit_record material_meta pointer) = 
object
  inherit hittable
  val mutable box_min = new vec3 [|0.0;0.0;0.0|]
  val mutable box_max = new vec3 [|0.0;0.0;0.0|]
  val mutable sides = new hittable_list [||]
  initializer 
    box_min <- p0;
    box_max <- p1;
    sides <- sides#add (new xy_rect p0#x p1#x p0#y p1#y p1#z m);
    sides <- sides#add (new xy_rect p0#x p1#x p0#y p1#y p0#z m);
    sides <- sides#add (new xz_rect p0#x p1#x p0#z p1#z p1#y m);
    sides <- sides#add (new xz_rect p0#x p1#x p0#z p1#z p0#y m);
    sides <- sides#add (new yz_rect p0#y p1#y p0#z p1#z p1#x m);
    sides <- sides#add (new yz_rect p0#y p1#y p0#z p1#z p0#x m);
  method hit (r : ray) (t_min : float) (t_max : float)
    (rcd : hit_record pointer) =
    sides#hit r t_min t_max rcd
  method bounding_box _ _ output_box = 
    output_box ^:= new aabb box_min box_max;
    true
end