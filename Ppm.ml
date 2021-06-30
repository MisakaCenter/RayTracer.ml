open Base
open Core
open Stdlib
open Color
open Vec
(* PPM Format *)

type ppm = {
  ppm_mode : string;
  pic_width : int;
  pic_height : int;
  max_color : int;
  content : vec3 array array;
}

let output pm target_file =
  let outc = Out_channel.create target_file in
  Printf.fprintf outc "%s\n%d %d\n%d\n" pm.ppm_mode pm.pic_width pm.pic_height
    pm.max_color;
  output_ppm_matrix pm.max_color pm.content outc;
  Out_channel.close outc