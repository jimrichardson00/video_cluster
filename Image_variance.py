import numpy as np
import cv2
from matplotlib import pyplot as plt
import os
import math
from decimal import *
getcontext().prec = 1000

# --------------------------------------------

def Image_variance( frame ):

  height, width, depth = frame.shape

  zer = frame[:, :, 0]
  one = frame[:, :, 1]
  two = frame[:, :, 2]

  mean_zer = int(round(np.mean(zer)))
  mean_one = int(round(np.mean(one)))
  mean_two = int(round(np.mean(two)))

  mean_frame = np.zeros((height, width, 3), np.uint8)

  mean_frame[:, :, 0] = mean_zer
  mean_frame[:, :, 1] = mean_one
  mean_frame[:, :, 2] = mean_two

  s = np.sum((frame[:,:,0:3] - mean_frame[:,:,0:3])**2)
  v = s/(3*height*width)

  return v
