import cv2
import numpy
import os
import os.path
import re
import shutil

video_dir = '/home/jim/Desktop/pca_video/video'
data_dir = '/home/jim/Desktop/pca_video/data'
frames_dir = '/home/jim/Desktop/pca_video/frames'

for filename in os.listdir(video_dir):

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