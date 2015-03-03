import numpy as np
import cv2
from matplotlib import pyplot as plt
import os
import math
from decimal import *
getcontext().prec = 1000

# --------------------------------------------

def Detect_features( img, to_dir, jpg_name ) :

  hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
  img2 = hsv
  img3 = hsv

  hue = hsv[:, :, 0]
  sat = hsv[:, :, 1]
  val = hsv[:, :, 2]

  cutoff = 130

  c1 = np.empty(hue.shape)
  c1.fill(255 - float(cutoff*255/180))
  c2 = np.empty(hue.shape)
  c2.fill(255)

  hue = float(255/180)*hue
  hue[hue <= float(cutoff*255/180)] = hue[hue <= float(cutoff*255/180)] + c1[hue <= float(cutoff*255/180)]
  hue[hue > float(cutoff*255/180)] = c2[hue > float(cutoff*255/180)] - hue[hue > float(cutoff*255/180)]

  reduce_col = Reduce_colors( hue, 10 )
  edges = cv2.Canny(reduce_col, 2*30, 30*4)
  kernel = np.ones((2, 2), np.uint8)
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
  
  max_area = str(int(round(max_area))).zfill(10)

  count = 4

  # img = cv2.cvtColor(img, cv2.COLOR_HSV2BGR)
  img2 = cv2.cvtColor(img2, cv2.COLOR_HSV2BGR)
  img3 = cv2.cvtColor(img3, cv2.COLOR_HSV2BGR)

  img_o = cv2.addWeighted(img, (Decimal(1)/Decimal(count))
    , img2, Decimal(1) - (Decimal(1)/Decimal(count)), 0)

  os.chdir(to_dir)
  cv2.imwrite(max_area + jpg_name + '02' + '.jpg', reduce_col)
  cv2.imwrite(max_area + jpg_name + '05' + '.jpg', img_o)

  return areas

def Features( video_file, W, H, video_dir, skip, to_dir ):

  img = Mean_frame( video_file, W, H, video_dir, skip )
  jpg_name = video_file + ".jpg"
  Detect_features( img, to_dir, jpg_name )











