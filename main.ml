open Base
open Stdlib
open Array
open Core
open Vec
open Ppm
open Ray
open Sphere
open Hittable
open Hittable_list
open Utils

(* Image *)
let aspect_ratio = 16.0 /. 9.0

let image_width : int = 400

let image_height : int = int_of_float (float_of_int image_width /. aspect_ratio)

let color : int = 255

let max_depth : int = 50

let focal_length : float = 1.0

let samples_per_pixel = 20

let target_file : string = "./output/out.ppm"

(* Camera *)

let viewport_height = 2.0

let viewport_width = aspect_ratio *. viewport_height

let origin = new vec3 [| 0.0; 0.0; 0.0 |]

let horizontal = new vec3 [| viewport_width; 0.0; 0.0 |]

let vertical = new vec3 [| 0.0; viewport_height; 0.0 |]

let lower_left_corner =
  origin -| (horizontal /= 2.0) -| (vertical /= 2.0)
  -| new vec3 [| 0.0; 0.0; focal_length |]

let get_ray u v : ray =
  new ray
    origin
    (lower_left_corner +| (horizontal *= u) +| (vertical *= v) -| origin)

(* Ray Color *)

let rec ray_color (r : ray) (world : hittable_list) (depth : int) =
  if depth <= 0 then new vec3 [| 0.0; 0.0; 0.0 |]
  else
    let rcd =
      new_pointer
        {
          p = new vec3 [||];
          normal = new vec3 [||];
          t = 0.0;
          front_face = false;
          mat_ptr = new_pointer (new lambertian (new vec3 [||]));
        }
    in
    if world#hit r 0.001 infinity rcd then
      let scattered = new_pointer (new ray (new vec3 [||]) (new vec3 [||])) in
      let attenuation = new_pointer (new vec3 [| 1.0; 1.0; 1.0 |]) in
      if !^(!^rcd.mat_ptr)#scatter r !^rcd attenuation scattered then
        !^attenuation *| ray_color !^scattered world (depth - 1)
      else new vec3 [| 0.0; 0.0; 0.0 |]
    else
      let unit_direction = unit_vector r#direction in
      let t = (unit_direction#y +. 1.0) *. 0.5 in
      (new vec3 [| 1.0; 1.0; 1.0 |] *= (1.0 -. t))
      +| (new vec3 [| 0.5; 0.7; 1.0 |] *= t)

(* World *)
let material_ground =
  new_pointer (new lambertian (new vec3 [| 0.8; 0.8; 0.0 |]))

let material_center =
  new_pointer (new lambertian (new vec3 [| 0.7; 0.3; 0.3 |]))

let material_left = new_pointer (new metal (new vec3 [| 0.8; 0.8; 0.8 |]) 0.3)

let material_right = new_pointer (new metal (new vec3 [| 0.8; 0.6; 0.2 |]) 1.0)

let world =
  new hittable_list
    [|
      new sphere (new vec3 [| 0.0; -100.5; -1.0 |]) 100.0 material_ground;
      new sphere (new vec3 [| 0.0; 0.0; -1.0 |]) 0.5 material_center;
      new sphere (new vec3 [| 1.0; 0.0; -1.0 |]) 0.5 material_right;
      new sphere (new vec3 [| -1.0; 0.0; -1.0 |]) 0.5 material_left;
    |]

(* Render *)
let basic (content : vec3 array array) : vec3 array array =
  for j = image_height - 1 downto 0 do
    if j % 20 = 0 then printf "Remaining: %d\n" j;
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
