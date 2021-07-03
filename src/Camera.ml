open Setting
open Raytracerml.Vec
open Raytracerml.Ray
open Raytracerml.Utils

let theta = degrees_to_radians vfov

let h = tan (theta /. 2.0)

let viewport_height = 2.0 *. h

let viewport_width = aspect_ratio *. viewport_height

let w = unit_vector (lookfrom -| lookat)

let u = unit_vector (cross vup w)

let v = cross w u
let origin = lookfrom

let horizontal = (u *= viewport_width) *= focus_dist

let vertical = (v *= viewport_height) *= focus_dist

let lower_left_corner =
  origin -| (horizontal /= 2.0) -| (vertical /= 2.0) -| (w *= focus_dist) 

let lens_radius = aperture /. 2.0

let get_ray s t : ray =
  let rd = (random_in_unit_disk 1) *= lens_radius in
  let offset = (u *= rd#x) +| (v *= rd#y) in
  new ray
    (origin +| offset)
    (lower_left_corner +| (horizontal *= s) +| (vertical *= t) -| origin -| offset)