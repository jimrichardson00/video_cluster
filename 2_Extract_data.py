import cv2
import numpy
import os
import os.path
import re
import shutil
import sys
import math
from decimal import *


video_dir = '/home/jim/Desktop/video_cluster/video'
data_dir = '/home/jim/Desktop/video_cluster/data'
frames_dir = '/home/jim/Desktop/video_cluster/frames'

def extract_data( filename ):

  #sets the full path of each folder
  filename_path = os.path.abspath(os.path.join(video_dir,filename))

  #sets root and extension of each filename
  fileRoot, fileExtension = os.path.splitext(filename)

  print filename
  print fileRoot
  print fileExtension

  if fileExtension == '.MP4':

    # change working directory
    os.chdir(video_dir)
    vidcap = cv2.VideoCapture(filename)

    success, image = vidcap.read()

    f = 0;
    while success:

      success, image = vidcap.read()
      
      if f % 30 == 0:

        height, width, depth = image.shape
        # print height, width, depth

        x1 = math.floor(Decimal(0.26041666666)*Decimal(width))
        y1 = math.floor(Decimal(0.55555555555)*Decimal(height))

        x2 = math.floor(Decimal(0.73958333334)*Decimal(width))
        y2 = math.floor(Decimal(0.55555555555)*Decimal(height))

        image = image[0:y1, x1:x2]
        image = cv2.resize(image, (192, 108))

        # output frames
        os.chdir(frames_dir)
        cv2.imwrite(fileRoot + "_" + "F" + str(f).zfill(4) + '.jpg', image)

        # output text files
        os.chdir(data_dir)
        numpy.savetxt(fileRoot + "_" + "F" + str(f).zfill(4) + "_" + 'blu' + '.txt', image[:,:,0])
        numpy.savetxt(fileRoot + "_" + "F" + str(f).zfill(4) + "_" + 'gre' + '.txt', image[:,:,1])
        numpy.savetxt(fileRoot + "_" + "F" + str(f).zfill(4) + "_" + 'red' + '.txt', image[:,:,2])

      f += 1

def main():

  if len(sys.argv) != 2:
    print 'usage: python 2_Extract_data.py FILENAME.MP4'
    sys.exit(1)

  filename = sys.argv[1]
  extract_data( filename )
  sys.exit(1)

if __name__ == '__main__':
  main()