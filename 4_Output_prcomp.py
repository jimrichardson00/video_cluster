import cv2
import numpy
import os
import os.path
import re
import shutil
import sys
import math
from decimal import *

def output_prcomp( n ):

  for i in range(1, int(n)):

    image_pc_blu = numpy.loadtxt("image_pc" + str(i) + "_blu.txt")
    image_pc_gre = numpy.loadtxt("image_pc" + str(i) + "_gre.txt")
    image_pc_red = numpy.loadtxt("image_pc" + str(i) + "_red.txt")

    image_pc = numpy.zeros((108, 192, 3), numpy.uint8)

    image_pc[:, :, 0] = image_pc_blu
    image_pc[:, :, 1] = image_pc_gre
    image_pc[:, :, 2] = image_pc_red

    cv2.imwrite("image_pc" + str(i) + ".jpg", image_pc)

def main():

  if len(sys.argv) != 2:
    print 'usage: python 4_Output_prcomp.py n'
    sys.exit(1)

  n = sys.argv[1]

  output_prcomp( n )
  
  sys.exit(1)

if __name__ == '__main__':
  main()