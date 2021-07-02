open Setting
open Vec
open Ray
open Utils

let theta = degrees_to_radians vfov

let h = tan (theta /. 2.0)

let viewport_height = 2.0 *. h

let viewport_width = aspect_ratio *. viewport_height

let w = unit_vector (lookfrom -| lookat)

let u = unit_vector (cross vup w)

let v = cross w u
let origin = lookfrom

let horizontal = u *= viewport_width

let vertical = v *= viewport_height

let lower_left_corner =
  origin -| (horizontal /= 2.0) -| (vertical /= 2.0) -| w

let get_ray s t : ray =
  new ray
    origin
    (lower_left_corner +| (horizontal *= s) +| (vertical *= t) -| origin)