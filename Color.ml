open Array
open Core
open Vec

let output_ppm_array (mc : int) (l : vec3 array) (outc : Out_channel.t) =
  iter
    (fun x ->
      Printf.fprintf outc "%s\n"
        (to_string
           ((fun yy ->
              new vec3
                [|
                  (float_of_int mc +. 0.99) *. yy#x;
                  (float_of_int mc +. 0.99) *. yy#y;
                  (float_of_int mc +. 0.99) *. yy#z;
                |])
              x)))
    l

let output_ppm_matrix mc m outc = iter (fun l -> output_ppm_array mc l outc) m
