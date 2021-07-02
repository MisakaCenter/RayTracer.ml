open Base
open Core
open Vec
open Ray

class camera (aspect_ratio_: float) (viewport_height_: float) (focal_length_: float) =
  object
    val mutable aspect_ratio = aspect_ratio_

    val mutable viewport_height = viewport_height_

    val mutable focal_length = focal_length_

    val mutable viewport_width = aspect_ratio_ *. viewport_height_

    val mutable origin = new vec3 [| 0.0; 0.0; 0.0 |]

    val mutable horizontal =
      new vec3 [| aspect_ratio_ *. viewport_height_; 0.0; 0.0 |]

    val mutable vertical = new vec3 [| 0.0; viewport_height_; 0.0 |]

    val mutable lower_left_corner =
      new vec3 [| 0.0; 0.0; 0.0 |]
      -| (new vec3 [| aspect_ratio_ *. viewport_height_; 0.0; 0.0 |] /= 2.0)
      -| (new vec3 [| 0.0; viewport_height_; 0.0 |] /= 2.0)
      -| new vec3 [| 0.0; 0.0; focal_length_ |]

    method get_ray (u:float) (v:float): ray = 
      new ray
          origin
          (lower_left_corner +| (horizontal *= u) +| (vertical *= v) -| origin)

  end
