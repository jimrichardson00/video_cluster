import cv2
from sklearn.decomposition import PCA
from pyIPCA import CCIPCA
from os.path import join, exists
import urllib
import zipfile
import sys
import os
from decimal import *
import math
import numpy

def Output_image( name, red, gre, blu, dest_dir, W, H ):

  image = numpy.zeros((H, W, 3), numpy.uint8)
  
  image[:, :, 0] = blu
  image[:, :, 1] = gre
  image[:, :, 2] = red

  image = numpy.clip(image, 0, 255)
  
  os.chdir(str(dest_dir))
  cv2.imwrite(str(name) + ".jpg", image)
