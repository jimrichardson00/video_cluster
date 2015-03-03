import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle

def Red_blob( video_file, frame_o ):

  # frame_o = cv2.resize(frame_o, (W, H))

  # smooth it
  frame_o = cv2.blur(frame_o, (3, 3))

  # convert to hsv and find range of colors
  hsv = cv2.cvtColor(frame_o, cv2.COLOR_BGR2HSV)
  # thresh1 = cv2.inRange(hsv, np.array((150, 10, 10)), np.array((180, 200, 255)))
  # thresh2 = cv2.inRange(hsv, np.array((0, 10, 10)), np.array((30, 200, 255)))
  thresh1 = cv2.inRange(hsv, np.array((150, 5, 5)), np.array((180, 255, 255)))
  thresh2 = cv2.inRange(hsv, np.array((0, 5, 5)), np.array((30, 255, 255)))
  thresh = thresh1 + thresh2
  thresh_c = thresh.copy()

  # find contours in the threshold image
  contours, hierarchy = cv2.findContours(thresh, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

  # finding contour with maximum area and store it as best_cnt
  max_area = 0
  for cnt in contours:
      area = cv2.contourArea(cnt)
      if area > max_area:
          max_area = area
          best_cnt = cnt

  res = cv2.bitwise_and(frame_o, frame_o, mask = thresh)
 
  print 'max area = ' + str(max_area)

  hue, sat, val = cv2.split(res)

  if np.nonzero(hue)[0].any() == False:
    hue0m = 0
  else:
    hue0 = hue[np.nonzero(hue)]
    hue0m = np.amin(hue0)

  if np.nonzero(sat)[0].any() == False:
    sat0m = 0
  else:
    sat0 = sat[np.nonzero(sat)]
    sat0m = np.amin(sat0)

  if np.nonzero(val)[0].any() == False:
    val0m = 0
  else:
    val0 = val[np.nonzero(val)]
    val0m = np.amin(val0)

  print "hue range " + str(hue0m) + "-" + str(np.amax(hue))
  print "sat range " + str(sat0m) + "-" + str(np.amax(sat))
  print "val range " + str(val0m) + "-" + str(np.amax(val))

  # # finding centroids of best_cnt and draw a circle there
  # M = cv2.moments(best_cnt)
  # cx, cy = int(M['m10']/M['m00']), int(M['m01']/M['m00'])
  # cv2.circle(frame_o, (cx,cy), 5, 255, -1)

  # Show it, if key pressed is 'Esc', exit the loop
  cv2.imshow('frame_o',frame_o)
  cv2.imshow('thresh', thresh_c)

  max_area_c = str(int(round(max_area, 0))).zfill(6)
  print max_area_c

  red_col = Reduce_colors( frame_o, 2 )

  os.chdir("/home/jim/Dropbox/REM/Tasks/video_cluster/coral2013")
  cv2.imwrite("O" + video_file + ".jpg", frame_o)
  # cv2.imwrite("T" + video_file + ".jpg", thresh_c)
  cv2.imwrite("R" + video_file + ".jpg", res)
  cv2.imwrite("C" + video_file + ".jpg", red_col)

  hsv_range = {}

  # print doesnt_exist

  hue = np.array(hue)
  hue = json.loads(json.dumps(hue.tolist()))

  sat = np.array(sat)
  sat = json.loads(json.dumps(sat.tolist()))

  val = np.array(val)
  val = json.loads(json.dumps(val.tolist()))

  hsv_range["hue"] = hue
  hsv_range["sat"] = sat
  hsv_range["val"] = val

  hsv_range["max_area"] = max_area

  return hsv_range
