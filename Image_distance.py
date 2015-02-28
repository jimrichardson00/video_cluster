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

def Image_distance( video_file_frame1, video_file_frame2, video_dir, H, W ):

  frame_o1 = numpy.zeros((H, W, 3), numpy.uint8)
  frame_o2 = numpy.zeros((H, W, 3), numpy.uint8)

  match_F1 = re.search("(.+GOPR.+)_F0+([1-9][0-9]+)", video_file_frame1)
  match_F2 = re.search("(.+GOPR.+)_F0+([1-9][0-9]+)", video_file_frame2)

  if match_F1 and match_F2:
    video_file1 = match_F1.group(1) + ".mp4"
    video_file2 = match_F2.group(1) + ".mp4"
    F1 = int(match_F1.group(2))
    F2 = int(match_F2.group(2))

    # sets the full path of each folder
    video_file_path1 = os.path.abspath(os.path.join(video_dir, video_file1))
    video_file_path2 = os.path.abspath(os.path.join(video_dir, video_file2))

    #sets root and extension of each video_file
    fileRoot1, fileExtension1 = os.path.splitext(video_file1)
    fileRoot2, fileExtension2 = os.path.splitext(video_file2)

    # change working directory
    os.chdir(video_dir)
    vidcap1 = cv2.VideoCapture(video_file1)
    success, frame = vidcap1.read()
    frame_rate = int(round(vidcap1.get(5)))
    f = 0;
    while success:
      success, frame = vidcap1.read()
      if f == F1:
        height, width, depth = frame.shape
        x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
        x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
        y1 = 0
        y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
        frame = frame[y1:y2, x1:x2]
        frame_o1 = cv2.resize(frame, (W, H))
        frame_o1 = frame_o1.flatten("C").copy()
        frame_o1 = numpy.array(frame_o1)
        frame_o1 = numpy.clip(frame_o1, 0, 255)
      f += 1

    # change working directory
    os.chdir(video_dir)
    vidcap2 = cv2.VideoCapture(video_file2)
    success, frame = vidcap2.read()
    frame_rate = int(round(vidcap2.get(5)))
    f = 0;
    while success:
      success, frame = vidcap2.read()
      if f == F2:
        height, width, depth = frame.shape
        x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
        x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
        y1 = 0
        y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
        frame = frame[y1:y2, x1:x2]
        frame_o2 = cv2.resize(frame, (W, H))
        frame_o2 = frame_o2.flatten("C").copy()
        frame_o2 = numpy.array(frame_o2)
        frame_o2 = numpy.clip(frame_o2, 0, 255)
      f += 1

  dist = numpy.linalg.norm(frame_o1 - frame_o2)

  return dist






