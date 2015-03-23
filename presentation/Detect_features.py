import numpy as np
import cv2
from matplotlib import pyplot as plt
import os
import math
from decimal import *
getcontext().prec = 1000

# --------------------------------------------

def Reduce_colors( frame_o, K ):

  Z = frame_o.reshape((-1, 3))
  Z = np.float32(Z)

  # define criteria, number of clusters(K) and apply kmeans()
  criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
  ret, label, center = cv2.kmeans(Z, K, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)

  # Now convert back into uint8, and make original image
  center = np.uint8(center)
  res = center[label.flatten()]
  res2 = res.reshape((frame_o.shape))

  return res2

def Detect_features( img, to_dir, jpg_name ) :

  hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
  img2 = hsv
  img3 = hsv

  hue = hsv[:, :, 0]
  sat = hsv[:, :, 1]
  val = hsv[:, :, 2]

  cv2.imwrite(jpg_name + '07' + '.jpg', hue)
  cv2.imwrite(jpg_name + '08' + '.jpg', sat)
  cv2.imwrite(jpg_name + '09' + '.jpg', val)

  cutoff = 130

  c1 = np.empty(hue.shape)
  c1.fill(255 - float(cutoff*255/180))
  c2 = np.empty(hue.shape)
  c2.fill(255)

  hue = float(255/180)*hue
  hue[hue <= float(cutoff*255/180)] = hue[hue <= float(cutoff*255/180)] + c1[hue <= float(cutoff*255/180)]
  hue[hue > float(cutoff*255/180)] = c2[hue > float(cutoff*255/180)] - hue[hue > float(cutoff*255/180)]

  reduce_col = Reduce_colors( hue, 20 )
  edges = cv2.Canny(reduce_col, 2*30, 4*30)
  # kernel = np.ones((2, 2), np.uint8)
  kernel = np.ones((4, 4), np.uint8)
  dilation = cv2.dilate(edges, kernel, iterations = 1)

  mask = dilation
  mask_inv = cv2.bitwise_not(mask)
  hue_edge = cv2.bitwise_and(img, img, mask = mask_inv)

  # find contours in the threshold image
  # contours, hierarchy = cv2.findContours(mask_inv, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_NONE)
  contours, hierarchy = cv2.findContours(mask, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_NONE)

  # red
  cv2.drawContours(img, contours, -1, (0, 0, 254), -1)

  # finding contour with maximum area and store it as best_cnt
  max_area = 0
  areas = []

  if_best_cnt = False

  for i in range(0, len(contours)):

    cnt = contours[i]

    if hierarchy[0][i][3] >= 0 or True:
    # has parent contour
    # if hierarchy[0][i][3] >= 0:

      area = cv2.contourArea(cnt)
      areas.append(area)
      # green
      cv2.drawContours(img, [cnt], -1, (0, 254, 0), -1)

      if area > max_area:
          max_area = area
          best_cnt = cnt

      if_best_cnt = True

  # blue
  if if_best_cnt == True:
    cv2.drawContours(img, [best_cnt], -1, (254, 0, 0), -1)
  
  max_area = str(int(float(max_area))).zfill(10)

  count = 4

  # img = cv2.cvtColor(img, cv2.COLOR_HSV2BGR)
  img2 = cv2.cvtColor(img2, cv2.COLOR_HSV2BGR)
  img3 = cv2.cvtColor(img3, cv2.COLOR_HSV2BGR)

  img_o = cv2.addWeighted(img, (Decimal(1)/Decimal(count))
    , img2, Decimal(1) - (Decimal(1)/Decimal(count)), 0)

  os.chdir(to_dir)
  cv2.imwrite(jpg_name + '01' + '.jpg', reduce_col)
  cv2.imwrite(jpg_name + '02' + '.jpg', img_o)
  cv2.imwrite(jpg_name + '03' + '.jpg', edges)
  cv2.imwrite(jpg_name + '04' + '.jpg', mask)
  cv2.imwrite(jpg_name + '05' + '.jpg', dilation)
  cv2.imwrite(jpg_name + '06' + '.jpg', hue_edge)
  # cv2.imwrite(jpg_name + '07' + '.jpg', hue)
  # cv2.imwrite(jpg_name + '08' + '.jpg', sat)
  # cv2.imwrite(jpg_name + '09' + '.jpg', val)
  cv2.imwrite(jpg_name + '10' + '.jpg', mask_inv)

  return areas

# video_file = "STRS2013_S036T014_GOPR0347_TrapRemoved"
video_file = "STRS2013_S035T015_GOPR0280"
from_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/presentation"
to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/presentation"
jpg_name = video_file + ".jpg"
jpg_name = video_file

os.chdir("/home/jim/Dropbox/REM/Tasks/video_cluster/presentation")
img = cv2.imread(jpg_name + ".jpg")

Detect_features( img, to_dir, jpg_name )








