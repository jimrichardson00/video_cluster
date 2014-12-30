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

def CCIPCA_SetTrap( SetTrap, n_components, W, H ):

  video_files = []

  for video_file in os.listdir(video_dir):

    SetTrap_match = str(SetTrap) + ".+"
    match_ST = re.search(SetTrap_match, video_file)

    if match_ST:
      video_files.append(video_file)

  frames = []
  video_files_frames = []

  for video_file in video_files:

    # sets the full path of each folder
    video_file_path = os.path.abspath(os.path.join(video_dir, video_file))

    #sets root and extension of each video_file
    fileRoot, fileExtension = os.path.splitext(video_file)

    if fileExtension == '.MP4':

      # change working directory
      os.chdir(video_dir)
      vidcap = cv2.VideoCapture(video_file)
      print video_file

      success, frame = vidcap.read()

      f = 0;
      while success:

        success, frame = vidcap.read()
        
        if f % 30 == 0:

          height, width, depth = frame.shape
          x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
          x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
          y1 = 0
          y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
          frame = frame[y1:y2, x1:x2]

          frame = cv2.resize(frame, (W, H))

          frames.append(frame)
      
          video_files_frames.append(fileRoot + "_F" + str(f))
          print fileRoot + "_F" + str(f)

        f += 1

      print " "

  n_components = int(n_components)

  ccipca = CCIPCA(n_components = n_components)

  frames_rowvecs = []
  for (i, frame) in enumerate(frames):
    frames_rowvecs.append(frame.flatten("C").copy())
    ccipca = ccipca.fit(frame.flatten("C").copy())
  
  output = []

  framesData = numpy.vstack(frames_rowvecs)
  framesData = numpy.array(framesData)
  framesData = json.loads(json.dumps(framesData.tolist()))
  output.append(framesData)

  components_ = ccipca.components_
  components_ = numpy.array(components_)
  components_ = json.loads(json.dumps(components_.tolist()))
  output.append(components_)
  
  output.append(video_files_frames)

  return output
