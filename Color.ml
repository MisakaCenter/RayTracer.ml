open Array
open Core
open Vec

type pixel = vec3

let output_ppm_array (mc : int) (l : pixel array) (outc : Out_channel.t) =
  iter
    (fun x ->
      Printf.fprintf outc "%s\n"
        (to_string
           ((fun y ->
              new vec3
                [|
                  (float_of_int mc +. 0.99) *. y#x;
                  (float_of_int mc +. 0.99) *. y#y;
                  (float_of_int mc +. 0.99) *. y#z;
                |])
              x)))
    l

let output_ppm_matrix mc m outc = iter (fun l -> output_ppm_array mc l outc) m
