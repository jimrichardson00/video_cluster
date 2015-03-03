import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle

def Remove_trap( frame ):

  height, width, depth = frame.shape

  x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))

  frame = frame[y1:y2, x1:x2]

  return frame
