all:
	dune build 
	dune exec -- ./src/main.exe
	python3 ppm_to_png.py
clean:
	rm -rf ./output/*.ppm
	rm -rf ./output/*.png
	rm -rf ./_build