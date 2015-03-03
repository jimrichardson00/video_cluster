import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle

# ----------------------------------------------

def Resize_frame( frame, W, H ):

  frame = cv2.resize(frame, (W, H))

  return frame


