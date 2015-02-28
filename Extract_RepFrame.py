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
import numpy

def Extract_RepFrame( video_file, W, H, skip, fast, video_dir ):

  # sets the full path of each folder
  video_file_path = os.path.abspath(os.path.join(video_dir, video_file))

  #sets root and extension of each video_file
  fileRoot, fileExtension = os.path.splitext(video_file)

  if fileExtension == '.mp4':

    # change working directory
    os.chdir(video_dir)
    vidcap = cv2.VideoCapture(video_file)

    success, frame = vidcap.read()

    frame = numpy.zeros((H, W, 3), numpy.uint8)

    frame_rate = int(round(vidcap.get(5)))

    output = []

    output.append(frame_rate)

    if fast == 0:

      count = 0

      f = 0;
      frames = []

      while success:

        success, frame = vidcap.read()
        
        if f % 30 == 0 and f >= frame_rate*skip:

          # height, width, depth = frame.shape
          # x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
          # x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
          # y1 = 0
          # y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
          # frame = frame[y1:y2, x1:x2]

          frame = cv2.resize(frame, (W, H))
          frames.append(frame)

          count = count + 1

        f += 1

      frames_rowvecs = []
      for (i, frame) in enumerate(frames):
        frames_rowvecs.append(frame.flatten("C").copy())

      framesData = numpy.vstack(frames_rowvecs)
      framesData = numpy.array(framesData)

      # n_components = int(count)
      # ccipca = CCIPCA(n_components = n_components).fit(framesData)

      ccipca = CCIPCA(n_components = 2).fit(framesData)
   
      prx = ccipca.transform(framesData)
      prx = numpy.array(prx)      
      prx = json.loads(json.dumps(prx.tolist()))
      output.append(prx)

    return output

  else:

    return "Not at .mp4 file"
