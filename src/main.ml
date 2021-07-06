open Base
open Stdlib
open Array
open Core
open Setting
open Camera
open Raytracerml
open Raytracerml.Utils
open Raytracerml.Vec
open Raytracerml.Ppm

(* Ray Color *)

let rec ray_color (r : Ray.ray) (world : Hittable_list.hittable_list) (depth : int) =
  if depth <= 0 then new vec3 [| 0.0; 0.0; 0.0 |]
  else
    let rcd =
      new_pointer
       Hittable.{
          p = new vec3 [||];
          normal = new vec3 [||];
          t = 0.0;
          front_face = false;
          mat_ptr = new_pointer (new lambertian (new vec3 [||]));
        }
    in
    if world#hit r 0.001 infinity rcd then
      let scattered = new_pointer (new Ray.ray (new vec3 [||]) (new vec3 [||]) 0.0)  in
      let attenuation = new_pointer (new vec3 [| 1.0; 1.0; 1.0 |]) in
      if !^(!^rcd.mat_ptr)#scatter r !^rcd attenuation scattered then
        !^attenuation *| ray_color !^scattered world (depth - 1)
      else new vec3 [| 0.0; 0.0; 0.0 |]
    else
      let unit_direction = unit_vector r#direction in
      let t = (unit_direction#y +. 1.0) *. 0.5 in
      (new vec3 [| 1.0; 1.0; 1.0 |] *= (1.0 -. t))
      +| (new vec3 [| 0.5; 0.7; 1.0 |] *= t)

(* Render *)
let basic (content : vec3 array array) : vec3 array array =
  let t_start = Unix.gettimeofday () in
  for j = image_height - 1 downto 0 do
    for i = 0 to image_width - 1 do
      let pixel_color = new_pointer (new vec3 [| 0.0; 0.0; 0.0 |]) in

      for _ = 0 to samples_per_pixel do
        let u =
          (float_of_int i +. random_float 1.0)
          /. (float_of_int image_width -. 1.0)
        in
        let v =
          (float_of_int j +. random_float 1.0)
          /. (float_of_int image_height -. 1.0)
        in
        let r = get_ray u v in
        pixel_color ^:= (!^pixel_color +| ray_color r world max_depth)
      done;

      set (get content (image_height - j - 1)) i !^pixel_color
    done
  done;
  printf "Ray Tracing finished in %.1f s.\n" (Unix.gettimeofday () -. t_start);
  content

;;
output
  {
    ppm_mode = "P3";
    pic_width = image_width;
    pic_height = image_height;
    max_color = color;
    content =
      basic
        (make_matrix image_height image_width (new vec3 [| 0.0; 0.0; 0.0 |]));
  }
  target_file samples_per_pixel
