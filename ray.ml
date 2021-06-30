open Base;;
open Stdlib;;
(* open Stdio;; *)
open Array;;
open Core;;

let width: int = 256;;
let height: int = 256;;
let color: int = 255;;

let target_file: string = "out.ppm";;

(* PPM Format *)
type pixel =
{
  r: float;
  g: float;
  b: float
}

type ppm = 
{
  ppm_mode : string;
  pic_width: int;
  pic_height: int;
  max_color: int;
  content: pixel array array
}

let to_string p = 
  (Int.to_string (int_of_float p.r)) ^
  " " ^
  (Int.to_string (int_of_float p.g)) ^ 
  " " ^
  (Int.to_string (int_of_float p.b));;

let output_ppm_array mc l (outc: Out_channel.t) = 
  iter (fun x -> Printf.fprintf outc "%s\n" 
                        (to_string 
                          ((fun y -> { r = (float_of_int mc +. 0.99) *. y.r;
                                       g = (float_of_int mc +. 0.99) *. y.g;
                                       b = (float_of_int mc +. 0.99) *. y.b }
                                           ) x)
                        )
        ) l;;

let output_ppm_matrix mc m outc = 
  iter (fun l -> output_ppm_array mc l outc) m;;

let output pm = 
begin
  let outc = Out_channel.create target_file in begin
    Printf.fprintf outc "%s\n%d %d\n%d\n" pm.ppm_mode pm.pic_width pm.pic_height pm.max_color;
    output_ppm_matrix pm.max_color pm.content outc;
  end;
  Out_channel.close outc;
end;;

(* Basic View *)

let basic (content: pixel array array): pixel array array = 
  for i = height - 1 downto 0 do
    for j = 0 to width - 1 do
      set (get content (height - i - 1)) j 
        { r= (float_of_int i) /. (float_of_int (height - 1));
          g= (float_of_int j) /. (float_of_int (width - 1)); 
          b= 0.25; }
    done;
  done;
  content
;;


output {ppm_mode="P3";pic_width=width;pic_height=height;max_color=color; content= basic (make_matrix height width {r=0.0;g=0.0;b=0.0}) };;
