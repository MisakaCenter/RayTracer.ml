open Core
open Hittable
open Hittable_list
open Utils
open Vec
open Sphere
open Float
open Texture
open Aarect
open Box
(* World *)

let random_scene =
  let lst = new_pointer (new hittable_list [||]) in

  let checker =
    new checker_texture (new solid_color (new vec3 [| 0.2; 0.3; 0.1 |])) (new solid_color (new vec3 [| 0.9; 0.9; 0.9 |])) in 
  lst
  ^:= !^lst#add
        (new sphere (new vec3 [| 0.0; -1000.0; 0.0 |]) 1000.0 (new_pointer (new lambertian (new_pointer checker)))) ;

  for a = -11 to 10 do
    for b = -11 to 10 do
      let choose_mat = random_float 1.0 in
      let center =
        new vec3
          [|
            float_of_int a +. (0.9 * random_float 1.0);
            0.2;
            float_of_int b +. (0.9 * random_float 1.0);
          |]
      in

      if (center -| new vec3 [| 4.0; 0.2; 0.0 |])#length >. 0.9 then
        let sphere_material =
          new_pointer (new lambertian (new_pointer(new solid_color (new vec3 [| 0.5; 0.5; 0.5 |]))))
        in
        if choose_mat <. 0.7 then (
          (* diffuse *)
          let albedo = (random_vec_n_m 0.0 1.0) *| (random_vec_n_m 0.0 1.0) in
          sphere_material ^:= new lambertian (new_pointer(new solid_color albedo));
          let center2 = center +| new vec3 [|0.0; random_float_n_m 0.0 0.5; 0.0|] in
          lst ^:= !^lst#add (new moving_sphere center center2 0.0 1.0 0.2 sphere_material))
        else if choose_mat <. 0.85 then (
          (* metal *)
          let albedo = random_vec_n_m 0.5 1.0 in
          let fuzz = random_float_n_m 0.0 0.5 in
          sphere_material ^:= new metal albedo fuzz;
          lst ^:= !^lst#add (new sphere center 0.2 sphere_material))
        else if choose_mat <. 0.95 then (
          (* glass *)
          sphere_material ^:= new dielectric 1.5;
          lst ^:= !^lst#add (new sphere center 0.2 sphere_material))
        else (
          (* glass through *)
          sphere_material ^:= new dielectric 1.5;
          lst ^:= !^lst#add (new sphere center 0.2 sphere_material);
          lst ^:= !^lst#add (new sphere center (-0.18) sphere_material))
    done
  done;

  let material1 = new_pointer (new lambertian (new_pointer ((new solid_color (new vec3 [| 0.4; 0.2; 0.1 |]))))) in
  lst ^:= !^lst#add (new sphere (new vec3 [| -4.0; 1.0; 0.0 |]) 1.0 material1);

  let material2 = new_pointer (new metal (new vec3 [| 0.7; 0.6; 0.5 |]) 0.0) in
  lst ^:= !^lst#add (new sphere (new vec3 [| 4.0; 1.0; 0.0 |]) 1.0 material2);

  let material3 = new_pointer (new dielectric 1.5) in
  lst ^:= !^lst#add (new sphere (new vec3 [| 0.0; 1.0; 0.0 |]) 1.0 material3);

  !^ lst

  let sphere_sea =
    let lst = new_pointer (new hittable_list [||]) in
    for a = -3 to 3 do
      for b = -3 to 3 do
        for c = -3 to 3 do
          let choose_mat = random_float 1.0 in
          let center =
            new vec3
              [|
                float_of_int a +. (0.9 * random_float 1.0);
                float_of_int c +. (0.9 * random_float 1.0);
                float_of_int b +. (0.9 * random_float 1.0);
              |]
          in
            let sphere_material =
              new_pointer (new lambertian (new_pointer ((new solid_color (new vec3 [| 0.5; 0.5; 0.5 |])))))
            in
            if choose_mat <. 0.25 then (
              (* diffuse *)
              let albedo = (random_vec_n_m 0.0 1.0) *| (random_vec_n_m 0.0 1.0) in
              sphere_material ^:= new lambertian (new_pointer(new solid_color albedo));
              lst ^:= !^lst#add (new sphere center (0.2 +. random_float 0.2) sphere_material))
            else if choose_mat <. 0.5 then (
              (* metal *)
              let albedo = random_vec_n_m 0.5 1.0 in
              let fuzz = random_float_n_m 0.0 0.5 in
              sphere_material ^:= new metal albedo fuzz;
              lst ^:= !^lst#add (new sphere center (0.2 +. random_float 0.2) sphere_material))
            else if choose_mat <. 0.75 then (
              (* glass *)
              sphere_material ^:= new dielectric 1.5;
              lst ^:= !^lst#add (new sphere center (0.2 +. random_float 0.2) sphere_material))
            else (
              (* glass through *)
              sphere_material ^:= new dielectric 1.5;
              let r = (0.2 +. random_float 0.5) in
              lst ^:= !^lst#add (new sphere center r sphere_material);
              lst ^:= !^lst#add (new sphere center (-r +. 0.02) sphere_material))
        done
      done
    done;
    !^ lst

let simple_light =
  let lst = new_pointer (new hittable_list [||]) in
  let checker =
    new checker_texture (new solid_color (new vec3 [| 0.2; 0.3; 0.1 |])) (new solid_color (new vec3 [| 0.9; 0.9; 0.9 |])) in 
  lst
  ^:= !^lst#add
        (new sphere (new vec3 [| 0.0; -1000.0; 0.0 |]) 1000.0 (new_pointer (new lambertian (new_pointer checker)))) ;
  lst
  ^:= !^lst#add
        (new sphere (new vec3 [| 0.0; 2.0; 0.0 |]) 2.0 (new_pointer (new lambertian (new_pointer checker)))) ;
  let difflight = new diffuse_light (new_pointer (new solid_color (new vec3 [| 4.0; 4.0; 4.0 |]))) in
  lst
  ^:= !^lst#add
        (new xy_rect 3.0 5.0 1.0 3.0 (-2.0) (new_pointer difflight)) ;
  !^lst

let cornell_box =
  let lst = new_pointer (new hittable_list [||]) in
  let red = new_pointer (new lambertian (new_pointer (new solid_color (new vec3 [|0.65;0.05;0.05|])))) in
  let white = new_pointer (new lambertian (new_pointer (new solid_color (new vec3 [|0.73;0.73;0.73|])))) in
  let green = new_pointer (new lambertian (new_pointer (new solid_color (new vec3 [|0.12;0.45;0.15|])))) in
  let light = new_pointer (new diffuse_light (new_pointer (new solid_color (new vec3 [|15.0;15.0;15.0|])))) in
  lst ^:= !^lst#add (new yz_rect 0.0 555.0 0.0 555.0 555.0 green);
  lst ^:= !^lst#add (new yz_rect 0.0 555.0 0.0 555.0 0.0 red);
  lst ^:= !^lst#add (new xz_rect 213.0 343.0 227.0 332.0 554.0 light);
  lst ^:= !^lst#add (new xz_rect 0.0 555.0 0.0 555.0 0.0  white);
  lst ^:= !^lst#add (new xz_rect 0.0 555.0 0.0 555.0 555.0  white);
  lst ^:= !^lst#add (new xy_rect 0.0 555.0 0.0 555.0 555.0  white);
  let box1 = new_pointer (new box (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|165.0;330.0;165.0|]) white) in
  let box1_rot = new translate (new_pointer (new rotate_y box1 15.0)) (new vec3 [|265.0;0.0;295.0|]) in
  lst ^:= !^lst#add box1_rot;
  let box2 = new_pointer (new box (new vec3 [|0.0;0.0;0.0|]) (new vec3 [|165.0;165.0;165.0|]) white) in
  let box2_rot = new translate (new_pointer (new rotate_y box2 (-18.0))) (new vec3 [|130.0;0.0;65.0|]) in
  lst ^:= !^lst#add box2_rot;
  !^lst