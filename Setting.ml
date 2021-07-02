open Vec

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

(* vertical field-of-view in degrees *)
let vfov = 20.0

let lookfrom = new vec3 [|-2.0;2.0;1.0|]

let lookat = new vec3 [|0.0;0.0;-1.0|]

let vup = new vec3 [|0.0;1.0;0.0|]