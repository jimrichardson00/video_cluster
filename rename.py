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

video_dir = "/home/jim/Desktop/video_cluster/video2013"

video_files = os.listdir(video_dir)

for video_file in video_files:

  print video_file

  video_file_path = os.path.abspath(os.path.join(video_dir,video_file))

  #sets root and extension of each video_file
  fileRoot, fileExtension = os.path.splitext(video_file)

  match_ST = re.search(r'DFO2013(.+)', video_file_path)

  NewFilename = "STRS2013" + str(match_ST.group(1))
  print NewFilename

  # new filename path
  NewFilename_path = os.path.abspath(os.path.join(video_dir, NewFilename))
  print NewFilename_path

  # full path of copied file
  video_file_path = os.path.abspath(os.path.join(video_dir, video_file))
  print video_file_path

  # # renames the file
  os.rename(video_file_path, NewFilename_path)



