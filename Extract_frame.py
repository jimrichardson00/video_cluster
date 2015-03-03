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
import re

def Extract_frame(video_frame, frame_dir, W, H, skip):

	# frame_o = numpy.zeros((H, W, 3), numpy.uint8)
 #  frame_o = frame_o.flatten("C").copy()
 #  frame_o = json.loads(json.dumps(frame_o.tolist()))

  os.chdir(frame_dir)
  frame_o = cv2.imread(video_frame)

  height, width, depth = frame_o.shape
  x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
  frame_o = frame_o[y1:y2, x1:x2]

  frame_o = cv2.resize(frame_o, (W, H))
  
  frame_o = frame_o.flatten("C").copy()
  frame_o = numpy.array(frame_o)
  frame_o = numpy.clip(frame_o, 0, 255)
  frame_o = json.loads(json.dumps(frame_o.tolist()))

  return frame_o
