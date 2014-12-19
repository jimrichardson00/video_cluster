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

rep_dir = '/home/jim/Desktop/video_cluster/frames/rep_frames'
master_dir = '/home/jim/Desktop/video_cluster'

os.chdir(rep_dir)
rep_frames = os.listdir(rep_dir)

print rep_frames[0]

img = cv2.imread(rep_frames[0])

print len(rep_frames)

count = 2;
for rep_frame in rep_frames[1:70]:

  print rep_frame

  img_r = cv2.imread(rep_frame)


  img = cv2.addWeighted(img, Decimal(1) - (Decimal(1)/Decimal(count)),
   img_r, Decimal(1)/Decimal(count), 0)

  # img1 = cv2.addWeighted(img, 0.5, img_r, 0.5, 0)

  count = count + 1

os.chdir(master_dir)
cv2.imwrite('Overlay.jpg', img)

img2 = img

# cv2.imshow('img', img2)
# cv2.waitKey()

# img2[0:60, 50:192 - 50] = (0, 0, 0)
# cv2.imshow('img', img2)
# cv2.waitKey()

height, width, depth = img.shape
print height, width, depth

print math.floor(Decimal(0.253436)*Decimal(500))
