  #!/usr/bin/env python

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

print sys.version

def Hello_world( video_file, master_dir ):

  os.chdir(master_dir)

  print master_dir

  print video_file

  vidcap = cv2.VideoCapture(video_file)

  frame_rate = int(round(vidcap.get(5)))
  n_frames = vidcap.get(7)
  n_seconds = int(math.floor(n_frames/frame_rate))

  print n_seconds

  cv2.waitKey(1)

  return frame_rate

  return video_file

def print_thing( thing1 ):

  print thing1
  print thing2

print thing1
print thing2


