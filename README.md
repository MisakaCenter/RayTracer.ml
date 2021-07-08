
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

#### Result: (Intel i7-8750H CPU @ 2.20GHz) (Random Scene)

```
Ray Tracing finished in 1474.2 s.
```
Random Scene               |Sphere Sea                 |Sphere Sea (hd)
:-------------------------:|:-------------------------:|:-------------------------:
<img src="https://z3.ax1x.com/2021/07/03/R2tV3V.png" height = 200 alt="">  |  <img src="https://z3.ax1x.com/2021/07/03/RRdt9s.png" height = 200 alt="" > | <img src="https://z3.ax1x.com/2021/07/06/RoZRgO.png" height = 200 alt="" >


### Ray Tracing: The Next Week


#### Result: (Intel i7-8750H CPU @ 2.20GHz)

Bouncing spheres (2071.7 s)| Texture (531.9 s)         | Light (1654.9 s)
:-------------------------:|:-------------------------:|:-------------------------:
<img src="https://z3.ax1x.com/2021/07/06/RI43TK.png" height = 200 alt="">  |  <img src="https://z3.ax1x.com/2021/07/06/R7NOhV.png" height = 200 alt="">  | <img src="https://z3.ax1x.com/2021/07/07/RHL2uV.png" height = 200 alt="">

Empty Cornell box (437.5 s)|Cornell box with two blocks (958.3 s)|Standard Cornell box (1165.1 s)
:-------------------------:|:-------------------------:|:-------------------------:
<img src="https://z3.ax1x.com/2021/07/08/RXcGNt.png" height = 400 alt="">  |  <img src="https://z3.ax1x.com/2021/07/08/RXhn3t.png" height = 400 alt="">  | <img src="https://z3.ax1x.com/2021/07/09/RXbC0P.png" height = 400 alt="">