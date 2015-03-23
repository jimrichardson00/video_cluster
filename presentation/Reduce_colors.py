import numpy as np
import cv2
from matplotlib import pyplot as plt
import os
import math
from decimal import *
getcontext().prec = 1000

def Reduce_colors( frame_o, K ):

  Z = frame_o.reshape((-1, 3))
  Z = np.float32(Z)

  # define criteria, number of clusters(K) and apply kmeans()
  criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
  ret, label, center = cv2.kmeans(Z, K, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)

  # Now convert back into uint8, and make original image
  center = np.uint8(center)
  res = center[label.flatten()]
  res2 = res.reshape((frame_o.shape))

  return res2
