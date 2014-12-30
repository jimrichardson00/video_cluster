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

video_dir = '/home/jim/Desktop/video_cluster/video'
data_dir = '/home/jim/Desktop/video_cluster/data'
frames_dir = '/home/jim/Desktop/video_cluster/frames'

def CCIPCA_video( video_file, n_components, W, H, fast ):

  # sets the full path of each folder
  video_file_path = os.path.abspath(os.path.join(video_dir,video_file))

  #sets root and extension of each video_file
  fileRoot, fileExtension = os.path.splitext(video_file)

  # print video_file
  # print fileRoot
  # print fileExtension

  if fileExtension == '.MP4':

    # change working directory
    os.chdir(video_dir)
    vidcap = cv2.VideoCapture(video_file)

    success, frame = vidcap.read()

    if fast == 0:

      f = 0;
      frames = []
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

        f += 1

    elif fast == 1:

      f = 0;
      frames = []
      while success:

        success, frame = vidcap.read()
        
        if f == 10*30:

          height, width, depth = frame.shape
          x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
          x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
          y1 = 0
          y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
          frame = frame[y1:y2, x1:x2]

          frame = cv2.resize(frame, (W, H))

          frames.append(frame)

        f += 1


    frames_rowvecs = []
    for (i, frame) in enumerate(frames):
      frames_rowvecs.append(frame.flatten("C").copy())

    framesData = numpy.vstack(frames_rowvecs) 
    
    output = []

    framesData = numpy.array(framesData)
    framesData = json.loads(json.dumps(framesData.tolist()))
    output.append(framesData)

    if fast == 0:

      n_components = int(n_components)
      ccipca = CCIPCA(n_components = n_components).fit(framesData)

      components_ = ccipca.components_

      components_ = numpy.array(components_)
      components_ = json.loads(json.dumps(components_.tolist()))
      output.append(components_)

    return output
