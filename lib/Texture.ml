open Vec
open Core
open Float

class virtual texture =
  object
    method virtual value: float -> float -> vec3 -> vec3
  end

class solid_color (c : vec3) = 
  object
  inherit texture
  
  val mutable color_value = c
  
  method value _ _ _ = color_value
  end

class checker_texture (_even: texture) (_odd: texture) =
object
  inherit texture
  val mutable even = _even
  val mutable odd = _odd
  method value u v p =
    let sines = (sin (10.0 *. p#x)) *. (sin (10.0 *. p#y)) *. (sin (10.0 *. p#z)) in
    if (sines <. 0.0) then odd#value u v p else even#value u v p
end