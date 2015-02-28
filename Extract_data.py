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
import re

def Extract_data( video_file_frame, W, H, video_dir ):

  frame_o = numpy.zeros((H, W, 3), numpy.uint8)
  frame_o = json.loads(json.dumps(frame_o.tolist()))

  match_F = re.search("(.+GOPR.+)_F0+([1-9][0-9]+)", video_file_frame)

  if match_F:
    video_file = match_F.group(1) + ".mp4"
    F = int(match_F.group(2))

    # sets the full path of each folder
    video_file_path = os.path.abspath(os.path.join(video_dir, video_file))

    #sets root and extension of each video_file
    fileRoot, fileExtension = os.path.splitext(video_file)

    # change working directory
    os.chdir(video_dir)
    vidcap = cv2.VideoCapture(video_file)

    success, frame = vidcap.read()

    frame_rate = int(round(vidcap.get(5)))

    f = 0;
    while success:

      success, frame = vidcap.read()
      
      if f == F:

        height, width, depth = frame.shape
        x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
        x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
        y1 = 0
        y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
        frame = frame[y1:y2, x1:x2]

        frame_o = cv2.resize(frame, (W, H))

        mean = cv2.mean(frame_o)
        mean = (mean[0] + mean[1] + mean[2])/3
        mean = int(numpy.rint(mean))

        frame_o = frame_o.flatten("C").copy()
        frame_o = numpy.array(frame_o)
       
        # frame_o = frame_o - numpy.tile(mean, W*H*3) + numpy.tile(127, W*H*3) 

        frame_o = numpy.clip(frame_o, 0, 255)
        frame_o = json.loads(json.dumps(frame_o.tolist()))

      f += 1

  return frame
