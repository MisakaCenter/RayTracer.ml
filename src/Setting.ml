open Raytracerml.Vec
open Raytracerml.Hittable_list
open Raytracerml.World

(* Image *)
let aspect_ratio = 16.0 /. 9.0

let image_width : int = 2000

let image_height : int = int_of_float (float_of_int image_width /. aspect_ratio)

let color : int = 255

let max_depth : int = 50

let focal_length : float = 1.0

let samples_per_pixel = 400

let target_file : string = "./output/out.ppm"

(* Camera *)

(* vertical field-of-view in degrees *)
let vfov = 20.0

let lookfrom = new vec3 [|26.0;3.0;6.0|]

let lookat = new vec3 [|0.0;2.0;0.0|]

let vup = new vec3 [|0.0;1.0;0.0|]

let aperture = 0.1

let focus_dist = 10.0

let time0 = 0.0

let time1 = 1.0


(* world *)
(* let world: hittable_list = random_scene *)
let world: hittable_list = simple_light

let background = new vec3 [|0.0;0.0;0.0|]