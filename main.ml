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

let image_width : int = 1000

let image_height : int = int_of_float (float_of_int image_width /. aspect_ratio)

let color : int = 255

let target_file : string = "./output/out.ppm"

(* Camera *)

let viewport_height = 2.0

let viewport_width = aspect_ratio *. viewport_height

let focal_length = 1.0

let origin = new vec3 [| 0.0; 0.0; 0.0 |]

let horizontal = new vec3 [| viewport_width; 0.0; 0.0 |]

let vertical = new vec3 [| 0.0; viewport_height; 0.0 |]

let lower_left_corner =
  origin -| (horizontal /= 2.0) -| (vertical /= 2.0)
  -| new vec3 [| 0.0; 0.0; focal_length |]

(* Sphere

let hit_sphere (center : vec3) (radius : float) (r : ray) : float =
  let oc = r#origin -| center in
  let a = r#direction#length_squared in
  let half_b = dot oc r#direction in
  let c = oc#length_squared -. (radius *. radius) in
  let discriminant = (half_b *. half_b) -. ( a *. c) in
  if (Float.compare discriminant 0.0) < 0 then -1.0
  else (-. half_b  -. sqrt(discriminant)) /. (2.0 *. a) *)

(* Ray Color *)

let ray_color (r : ray) (world : hittable_list) =
  let rcd =
    new_pointer
      { p = new vec3 [||]; normal = new vec3 [||]; t = 0.0; front_face = false }
  in
  if !^(world#hit r 0.0 infinity rcd) then
    (!^rcd.normal +| new vec3 [| 1.0; 1.0; 1.0 |]) *= 0.5
  else
    let unit_direction = unit_vector r#direction in
    let t = (unit_direction#y +. 1.0) *. 0.5 in
    (new vec3 [| 1.0; 1.0; 1.0 |] *= (1.0 -. t))
    +| (new vec3 [| 0.5; 0.7; 1.0 |] *= t)

(* World *)
let world =
  new hittable_list
    [|
      new sphere (new vec3 [| 0.0; -100.5; -1.0 |]) 100.0;
      new sphere (new vec3 [| 0.0; 0.0; -1.0 |]) 0.5;
    |]

(* Render *)
let basic (content : vec3 array array) : vec3 array array =
  for j = image_height - 1 downto 0 do
    if j % 20 = 0 then printf "Remaining: %d\n" j;
    for i = 0 to image_width - 1 do
      let u = float_of_int i /. (float_of_int image_width -. 1.0) in
      let v = float_of_int j /. (float_of_int image_height -. 1.0) in
      let ray1 =
        new ray
          origin
          (lower_left_corner +| (horizontal *= u) +| (vertical *= v) -| origin)
      in

      set (get content (image_height - j - 1)) i (ray_color ray1 world)
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
  target_file
