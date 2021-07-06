
# RayTracer.ml

🌈 path tracer implemented in OCaml based on "[_Ray Tracing in One Weekend_](https://raytracing.github.io/books/RayTracingInOneWeekend.html)
"

## Usage

To render the scene, run the following command
``` 
make
```
 , and the rendered scene will be stored in ./output/

## Screenshots

### Ray Tracing in One Weekend

#### Settings:

- let samples_per_pixel = 100
- let max_depth : int = 50
- let aspect_ratio = 3.0 /. 2.0
- let image_width : int = 400

#### Result: (Intel i7-8750H CPU @ 2.20GHz)

```
Ray Tracing finished in 1474.2 s.
```
Random Scene               |  Sphere Sea
:-------------------------:|:-------------------------:
<img src="https://z3.ax1x.com/2021/07/03/R2tV3V.png" height = 300 alt="">  |  <img src="https://z3.ax1x.com/2021/07/03/RRdt9s.png" height = 300 alt="" >



### Ray Tracing: The Next Week

#### Settings:

- let samples_per_pixel = 100
- let max_depth : int = 50
- let aspect_ratio = 16.0 /. 2.0
- let image_width : int = 400

#### Result: (Intel i7-8750H CPU @ 2.20GHz)

```
Ray Tracing finished in 2071.7 s.
```

Bouncing spheres           |  *
:-------------------------:|:-------------------------:
<img src="https://z3.ax1x.com/2021/07/06/RI43TK.png" height = 300 alt="">  |  
