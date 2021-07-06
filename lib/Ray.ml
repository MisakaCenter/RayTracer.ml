open Vec
open Base

class ray (o : vec3) (d : vec3) (t : float) =
  object
    val mutable orig = o

    val mutable direc = d

    val mutable tim = t

    method origin = orig

    method direction = direc

    method time = tim
  end

let at (r : ray) (t : float) = r#origin +| (r#direction *= t)
