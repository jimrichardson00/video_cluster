import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle

# ----------------------------------------------

def Resize_frame( frame, W, H ):

  frame = cv2.resize(frame, (W, H))
  return frame

def Remove_trap( frame ):

  height, width, depth = frame.shape

  x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))

  frame = frame[y1:y2, x1:x2]

  return frame

def Mean_frame( video_file, W, H, video_dir, skip ):

  frame_o = np.zeros((W, H, 3))

  os.chdir(video_dir)
  vidcap = cv2.VideoCapture(video_file)
  frame_rate = int(round(vidcap.get(5)))

  success, frame = vidcap.read()

  f = 0
  count = 2

  while success:

    success, frame = vidcap.read()

    if success == True:

      frame = Remove_trap( frame )
      frame = Resize_frame( frame, W, H )

      if f >= frame_rate*skip and f % 1 == 0:

        if f == frame_rate*skip and f % 1 == 0:

          frame_o = frame

        else:

          frame_o = cv2.addWeighted(frame_o, Decimal(1) - (Decimal(1)/Decimal(count)),
                    frame, Decimal(1)/Decimal(count), 0)

          count = count + 1

    f = f + 1

  frame_o = frame_o.flatten("C").copy()
  frame_o = np.array(frame_o)
  frame_o = np.clip(frame_o, 0, 255)
  frame_o = json.loads(json.dumps(frame_o.tolist()))

  return frame_o

# -----------------------------------------------

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

# -----------------------------------------

# video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video2013"
# coral_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/coral2013"
# master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster"

# # video_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster/video2013"
# # coral_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster/coral2013"
# # master_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster"

# skip = 6

# W = 192
# H = 108

# # frame_o = Mean_frame( video_file )
# # frame_o = Remove_trap( frame_o )

# # Red_blob( video_file, frame_o )
# # hsv_range = Red_blob( video_file, frame_o )

# # # ------------------------------------

# # frames = []

# video_files = sorted(os.listdir(video_dir))
# for video_file in video_files:

#   print video_file

#   # calculate mean frame from video
#   os.chdir(video_dir)
#   frame_o =  Mean_frame( video_file, W, H, video_dir, skip )

#   # # remove trap
#   # frame_o = Remove_trap( frame_o)

#   # write to coral dir
#   os.chdir(coral_dir)
#   cv2.imwrite("O" + video_file + ".jpg", frame_o)

#   # # add to frames
#   # frames.append(frame_o)

# # os.chdir(master_dir)
# # pickle.dump(frames, open("frames.p", "wb"))

# # # ------------------------------------

# # os.chdir(master_dir)
# # frames = pickle.load(open("frames.p", "rb"))

# # frames_rowvecs = []

# # for (i, frame) in enumerate(frames):

# #   frames_rowvecs.append(frame.flatten("C").copy())

# # framesData = np.vstack(frames_rowvecs)
# # framesData = np.array(framesData)
# # framesData = np.float32(framesData)

# # K = 2

# # # define criteria, number of clusters(K) and apply kmeans()
# # criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
# # ret, label, center = cv2.kmeans(framesData, K, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)

# # print ret
# # print label
# # print center


