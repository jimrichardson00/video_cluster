import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math

def Red_blob( frame_o ):

  height, width, depth = frame_o.shape

  x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))

  frame_o = frame_o[y1:y2, x1:x2]
  # frame_o = cv2.resize(frame_o, (W, H))

  # smooth it
  frame_o = cv2.blur(frame_o, (3, 3))

  # convert to hsv and find range of colors
  hsv = cv2.cvtColor(frame_o,cv2.COLOR_BGR2HSV)
  thresh = cv2.inRange(hsv, np.array((120, 0, 0)), np.array((180, 100, 100)))
  thresh2 = thresh.copy()

  # find contours in the threshold image
  contours, hierarchy = cv2.findContours(thresh, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

  # finding contour with maximum area and store it as best_cnt
  max_area = 0
  for cnt in contours:
      area = cv2.contourArea(cnt)
      if area > max_area:
          max_area = area
          best_cnt = cnt

  print 'max area = ' + str(max_area)

  # # finding centroids of best_cnt and draw a circle there
  # M = cv2.moments(best_cnt)
  # cx, cy = int(M['m10']/M['m00']), int(M['m01']/M['m00'])
  # cv2.circle(frame_o, (cx,cy), 5, 255, -1)

  # Show it, if key pressed is 'Esc', exit the loop
  cv2.imshow('frame_o',frame_o)
  cv2.imshow('thresh',thresh2)

  cv2.imwrite("frame_o.jpg", frame_o)
  cv2.imwrite("thresh2.jpg", thresh2)


