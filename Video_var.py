#!/usr/bin/env python

"""eigenface
finds the first k eigenfaces using pca and inc pca
the results are displayed side by side for visual
comparison
"""
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
import re
import numpy

video_dir = '/home/jim/Desktop/video_cluster/video'
data_dir = '/home/jim/Desktop/video_cluster/data'
frames_dir = '/home/jim/Desktop/video_cluster/frames'

def Video_var( video_file, W, H, skip ):

  # sets the full path of each folder
  video_file_path = os.path.abspath(os.path.join(video_dir, video_file))

  #sets root and extension of each video_file
  fileRoot, fileExtension = os.path.splitext(video_file)

  if fileExtension == '.MP4':

    # change working directory
    os.chdir(video_dir)
    vidcap = cv2.VideoCapture(video_file)

    success, frame = vidcap.read()

    n = 0
    mean = numpy.zeros(3*W*H)
    M2 = numpy.zeros(3*W*H)

    frame_rate = int(round(vidcap.get(5)))

    f = 0;
    while success:

      success, frame = vidcap.read()
      
      if f % frame_rate == 0 and f >= frame_rate*skip:

        frame = cv2.resize(frame, (W, H))

        x = frame.flatten("C").copy()
     
        n = n + 1
        delta = x - mean
        mean = mean + delta/n
        M2 = M2 + delta*(x - mean)
          
      f += 1

  if fileExtension == '.MP4':
    if (n < 2):
      return 0
    else:
      return numpy.sum(M2)/(n - 1)
  else:
    return "Not an MP4 file"
    