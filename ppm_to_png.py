import os 
import cv2 
from glob import glob 

cwd = os.getcwd()
input_dir = os.path.join(cwd, "./output/*.ppm")    
ppms = glob(input_dir)

counter = 1 

for ppm in ppms: 
    cv2.imwrite("./output/"+str(counter)+".png", cv2.imread(ppm))
    counter += 1 