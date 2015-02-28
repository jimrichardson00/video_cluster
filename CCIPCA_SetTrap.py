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

def CCIPCA_SetTrap( SetTrap, W, H, skip, video_dir ):

  video_files = []

  for video_file in os.listdir(video_dir):

    SetTrap_match = str(SetTrap) + ".+"
    match_ST = re.search(SetTrap_match, video_file)

    if match_ST:
      video_files.append(video_file)

  frames = []
  video_files_frames = []

  count = 0

  for video_file in video_files:

    # sets the full path of each folder
    video_file_path = os.path.abspath(os.path.join(video_dir, video_file))

    #sets root and extension of each video_file
    fileRoot, fileExtension = os.path.splitext(video_file)

    if fileExtension == '.mp4':

      frame = numpy.zeros((H, W, 3), numpy.uint8)

      # change working directory
      os.chdir(video_dir)
      vidcap = cv2.VideoCapture(video_file)
      # print video_file

      success, frame = vidcap.read()

      frame_rate = int(round(vidcap.get(5)))

      f = 0;
      while success:

        success, frame = vidcap.read()
        
        if f % 30 == 0 and f >= frame_rate*skip:

          height, width, depth = frame.shape
          # x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
          # x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
          # y1 = 0
          # y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
          # frame = frame[y1:y2, x1:x2]

          frame = cv2.resize(frame, (W, H))

          frames.append(frame)
      
          video_files_frames.append(fileRoot + "_F" + str(f).zfill(5))
          # print fileRoot + "_F" + str(f).zfill(5)

          count = count + 1

        f += 1

      # print " "

  n_components = int(count)

  ccipca = CCIPCA(n_components = n_components)

  frames_rowvecs = []
  for (i, frame) in enumerate(frames):
    frames_rowvecs.append(frame.flatten("C").copy())
    ccipca = ccipca.fit(frame.flatten("C").copy())
  
  output = []

  framesData = numpy.vstack(frames_rowvecs)
  framesData = numpy.array(framesData)

  components_ = ccipca.components_
  components_ = numpy.array(components_)

  prx = components_.dot(framesData.T).T
  prx = numpy.array(prx)
  prx = json.loads(json.dumps(prx.tolist()))
  output.append(prx)

  output.append(video_files_frames)

  output.append(count)

  # framesData = json.loads(json.dumps(framesData.tolist()))
  # output.append(framesData)

  # components_ = json.loads(json.dumps(components_.tolist()))
  # output.append(components_)

  return output

