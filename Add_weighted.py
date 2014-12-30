import cv2
import numpy
import os
import os.path
import re
import shutil
import sys
import math
from decimal import *
getcontext().prec = 6

def Add_weighted( video_dir, master_dir, W, H ):

  os.chdir(video_dir)
  video_files = os.listdir(video_dir)

  # define image_o
  vidcap = cv2.VideoCapture(video_files[0])
  success, image = vidcap.read()

  f = 0;
  while success:
    success, image = vidcap.read()
    # takes the frame at the 10 second mark
    if f == 30*10:
      image_o = image
      image_o = cv2.resize(image_o, (W, H))
    f += 1

  # cycle through images
  count = 2;
  for video_file in video_files[1:len(video_files)-1]:

    #sets the full path of each folder
    video_file_path = os.path.abspath(os.path.join(video_dir,video_file))

    #sets root and extension of each video_file
    fileRoot, fileExtension = os.path.splitext(video_file_path)

    if fileExtension == '.MP4':

      # change working directory
      os.chdir(video_dir)
      vidcap = cv2.VideoCapture(video_file)

      success, image = vidcap.read()

      f = 0;
      while success:

        success, image = vidcap.read()
      
        if f == 30*10:

          image = cv2.resize(image, (W, H))

          image_o = cv2.addWeighted(image_o, Decimal(1) - (Decimal(1)/Decimal(count)),
          	image, Decimal(1)/Decimal(count), 0)

        f += 1

    count = count + 1

  os.chdir(master_dir)

  cv2.imwrite('image_o' + str(count - 1).zfill(4) + '.jpg', image_o)
  
  height, width, depth = image_o.shape

  x1 = math.floor(Decimal(0.26041666666)*Decimal(width))
  x2 = math.floor(Decimal(0.73958333334)*Decimal(width))

  y1 = 0
  y2 = math.floor(Decimal(0.65555555555)*Decimal(height))

  image_o[y1:y2, x1:x2] = (0, 0, 0)
  cv2.imwrite('image_o' + str(count - 1).zfill(4) + '_floor.jpg', image_o)
