open Raytracerml.Vec
open Raytracerml.Hittable_list
open Raytracerml.World

(* Image *)
let aspect_ratio = 1.0 /. 1.0

let image_width : int = 600

let image_height : int = int_of_float (float_of_int image_width /. aspect_ratio)

let color : int = 255

let max_depth : int = 50

let focal_length : float = 1.0

let samples_per_pixel = 200

let target_file : string = "./output/out.ppm"

(* Camera *)

(* vertical field-of-view in degrees *)
let vfov = 40.0

let lookfrom = new vec3 [|278.0;278.0;-800.0|]

let lookat = new vec3 [|278.0;278.0;0.0|]

let vup = new vec3 [|0.0;1.0;0.0|]

let aperture = 0.1

let focus_dist = 10.0

let time0 = 0.0

let time1 = 1.0


(* world *)
(* let world: hittable_list = random_scene *)
let world: hittable_list = cornell_box

let background = new vec3 [|0.0;0.0;0.0|]