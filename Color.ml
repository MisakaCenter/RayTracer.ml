open Array
open Core
open Vec
open Utils

let output_ppm_array (mc : int) (l : vec3 array) (outc : Out_channel.t) (samples_per_pixel: int) =
  iter
    (fun x ->
      Printf.fprintf outc "%s\n"
        (to_string
           ((fun yy ->
            let r =  (yy#x /. float_of_int samples_per_pixel) ** (1. /. 2.2) in
            let g =  (yy#y /. float_of_int samples_per_pixel)** (1. /. 2.2) in
            let b = (yy#z /. float_of_int samples_per_pixel)** (1. /. 2.2) in
              new vec3
                [|
                  (float_of_int mc +. 0.999) *. (clamp r 0.0 0.999);
                  (float_of_int mc +. 0.999) *. (clamp g 0.0 0.999);
                  (float_of_int mc +. 0.999) *. (clamp b 0.0 0.999);
                |])
              x)))
    l

let output_ppm_matrix mc m outc i = iter (fun l -> output_ppm_array mc l outc i) m
