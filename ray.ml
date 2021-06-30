open Base
open Stdlib

(* open Stdio;; *)
open Array
open Core
open Vec
open Color

let width : int = 256

let height : int = 256

let color : int = 255

let target_file : string = "./output/out.ppm"

(* PPM Format *)

type ppm = {
  ppm_mode : string;
  pic_width : int;
  pic_height : int;
  max_color : int;
  content : pixel array array;
}

let output pm =
  let outc = Out_channel.create target_file in
  Printf.fprintf outc "%s\n%d %d\n%d\n" pm.ppm_mode pm.pic_width pm.pic_height
    pm.max_color;
  output_ppm_matrix pm.max_color pm.content outc;
  Out_channel.close outc

(* Basic View *)

let basic (content : pixel array array) : pixel array array =
  for i = height - 1 downto 0 do
    if i % 20 = 0 then printf "Remaining: %d\n" i;
    for j = 0 to width - 1 do
      set
        (get content (height - i - 1))
        j
        (new vec3
           [|
             float_of_int i /. float_of_int (height - 1);
             float_of_int j /. float_of_int (width - 1);
             0.25;
           |])
    done
  done;
  content

;;
output
  {
    ppm_mode = "P3";
    pic_width = width;
    pic_height = height;
    max_color = color;
    content = basic (make_matrix height width (new vec3 [| 0.0; 0.0; 0.0 |]));
  }
