open Vec
open Base

class ray (o : vec3) (d : vec3) =
  object
    val mutable orig = o

    val mutable direc = d

    method origin = orig

    method direction = direc
  end

let at (r : ray) (t : float) =  r#origin +| (r#direction *= t)
