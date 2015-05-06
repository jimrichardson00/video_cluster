import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle

def Remove_trap( frame, x1, x2, y1, y2 ):

  height, width, depth = frame.shape

  x1 = int(round(Decimal(x1)*Decimal(width)))
  x2 = int(round(Decimal(x2)*Decimal(width)))
  y1 = int(round(Decimal(1.0 - y2)*Decimal(height)))
  y2 = int(round(Decimal(1.0 - y1)*Decimal(height)))

  frame = frame[y1:y2, x1:x2]

  return frame
