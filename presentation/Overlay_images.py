import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle
skip = 6

W = 920
H = 708

def Remove_trap( frame ):

  height, width, depth = frame.shape

  x1 = int(round(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(round(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(round(Decimal(0.65555555555)*Decimal(height)))

  frame = frame[y1:y2, x1:x2]

  return frame

cap = cv2.VideoCapture('STRS2013_S036T014_GOPR0347.MP4')

# Define the codec and create VideoWriter object
fourcc = cv2.cv.CV_FOURCC(*'XVID')
out_o = cv2.VideoWriter('output_o.avi',fourcc, 30, (1920/3, 1080/3))
out_n = cv2.VideoWriter('output_n.avi',fourcc, 30, (1920/3, 1080/3))

f = 0
count = 2
frame_rate = 30

while(cap.isOpened()):

  ret, frame = cap.read()
  # frame = Remove_trap( frame )
  frame = cv2.resize(frame, (1920/3, 1080/3))

  if ret == True:

    if f >= frame_rate*skip and f % 1 == 0 and f <= frame_rate*8:

      if f == frame_rate*skip and f % 1 == 0:

        frame_o = frame

      else:

        frame_o = cv2.addWeighted(frame_o, Decimal(1) - (Decimal(1)/Decimal(count)),
                  frame, Decimal(1)/Decimal(count), 0)
      
      out_o.write(frame_o)
      out_n.write(frame)

      cv2.imshow('frame',frame)
      if cv2.waitKey(1) & 0xFF == ord('q'):
          break
      
      count = count + 1

  f = f + 1

# Release everything if job is finished
cap.release()
out_o.release()
out_n.release()
cv2.destroyAllWindows()