open Array
open Core
open Vec
open Utils

let output_ppm_array (mc : int) (l : vec3 array) (outc : Out_channel.t) (samples_per_pixel: int)=
  iter
    (fun x ->
      Printf.fprintf outc "%s\n"
        (to_string
           ((fun yy ->
            let scale = 1.0 /. (float_of_int samples_per_pixel) in
            let r = yy#x *. scale 
            and g = yy#y *. scale 
            and b = yy#z *. scale in
              new vec3
                [|
                  (float_of_int mc +. 0.99) *.  (clamp r  0.0  0.999);
                  (float_of_int mc +. 0.99) *.  (clamp g  0.0  0.999);
                  (float_of_int mc +. 0.99) *. ( clamp b  0.0  0.999);
                |])
              x)))
    l

let output_ppm_matrix mc m outc a = iter (fun l -> output_ppm_array mc l outc a ) m
