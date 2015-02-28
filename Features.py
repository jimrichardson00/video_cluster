import numpy as np
import cv2
from matplotlib import pyplot as plt
import os
import math
from decimal import *
getcontext().prec = 1000


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

# ---------------------------------------------

def Remove_trap( frame ):

  height, width, depth = frame.shape

  x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))

  frame = frame[y1:y2, x1:x2]

  return frame

# --------------------------------------------

def Image_variance( frame ):

  height, width, depth = frame.shape

  zer = frame[:, :, 0]
  one = frame[:, :, 1]
  two = frame[:, :, 2]

  mean_zer = int(round(np.mean(zer)))
  mean_one = int(round(np.mean(one)))
  mean_two = int(round(np.mean(two)))

  mean_frame = np.zeros((height, width, 3), np.uint8)

  mean_frame[:, :, 0] = mean_zer
  mean_frame[:, :, 1] = mean_one
  mean_frame[:, :, 2] = mean_two

  s = np.sum((frame[:,:,0:3] - mean_frame[:,:,0:3])**2)
  v = s/(3*height*width)

  return v

# --------------------------------------------

def Color_bob( img, to_dir, jpg_name ) :

  # os.chdir(master_dir)
  # img = cv2.imread(jpg_name)

  hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
  img2 = hsv
  img3 = hsv

  hue = hsv[:, :, 0]
  sat = hsv[:, :, 1]
  val = hsv[:, :, 2]

  # img_rgb = img
  # img_rgb[:, :, 0] = hue
  # img_rgb[:, :, 1] = sat
  # img_rgb[:, :, 2] = val
  # os.chdir(to_dir)
  # cv2.imwrite(jpg_name + '06' + '.jpg', img_rgb)

  cutoff = 130

  c1 = np.empty(hue.shape)
  c1.fill(255 - float(cutoff*255/180))
  c2 = np.empty(hue.shape)
  c2.fill(255)

  hue = float(255/180)*hue
  hue[hue <= float(cutoff*255/180)] = hue[hue <= float(cutoff*255/180)] + c1[hue <= float(cutoff*255/180)]
  hue[hue > float(cutoff*255/180)] = c2[hue > float(cutoff*255/180)] - hue[hue > float(cutoff*255/180)]

  reduce_col = Reduce_colors( hue, 10 )
  # blur = cv2.bilateralFilter(reduce_col, 15, 75, 75)
  # blur = cv2.bilateralFilter(hue, 10, 75, 75)
  # blur = cv2.bilateralFilter(hue, 10, 10, 20)
  # blur = cv2.bilateralFilter(hue, 15, 30, 30)
  # reduce_col = Reduce_colors( blur, 10 )
  edges = cv2.Canny(reduce_col, 2*30, 30*4)
  # edges = cv2.Canny(hue, 0, 30*4)
  # edges = cv2.Canny(blur, 0, 30*4)
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

  if_best_cnt = False

  for i in range(0, len(contours)):

    cnt = contours[i]

    if hierarchy[0][i][3] >= 0 or True:
    # has parent contour
    # if hierarchy[0][i][3] >= 0:

      area = cv2.contourArea(cnt)
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
  # cv2.imwrite(jpg_name + '01' + '.jpg', img)
  # cv2.imwrite(jpg_name + '01' + '.jpg', hue)
  cv2.imwrite(max_area + jpg_name + '02' + '.jpg', reduce_col)
  # cv2.imwrite(jpg_name + '03' + '.jpg', dilation)
  # cv2.imwrite(max_area + jpg_name + '04' + '.jpg', hue_edge)
  cv2.imwrite(max_area + jpg_name + '05' + '.jpg', img_o)
  # cv2.imwrite(max_area + jpg_name + '06' + '.jpg', img_rgb)
  # cv2.imwrite(max_area + jpg_name + '07' + '.jpg', img3)
  cv2.imwrite(jpg_name + '06' + '.jpg', mask)
  cv2.imwrite(jpg_name + '08' + '.jpg', hue_edge)

# --------------------------------------------

def Mean_frame( video_file, W, H, video_dir, skip ):

  os.chdir(video_dir)
  vidcap = cv2.VideoCapture(video_file)
  frame_rate = int(round(vidcap.get(5)))

  frame_o = np.zeros((W, H, 3))

  success, frame = vidcap.read()

  f = 0
  count = 2

  while success:

    success, frame = vidcap.read()

    if success == True:

      frame = Remove_trap( frame )
      # frame = Resize_frame( frame, W, H )

      if f >= frame_rate*skip:

        if f == frame_rate*skip:

          frame_o = frame

        else:

          frame_o = cv2.addWeighted(frame_o, Decimal(1) - (Decimal(1)/Decimal(count)),
                    frame, Decimal(1)/Decimal(count), 0)

          count = count + 1

    f = f + 1

  # frame_o = frame_o.flatten("C").copy()
  # frame_o = np.array(frame_o)
  # frame_o = np.clip(frame_o, 0, 255)
  # frame_o = json.loads(json.dumps(frame_o.tolist()))

  return frame_o

# --------------------------------------------

# jpg_names = sorted(os.listdir("/home/jim/Dropbox/REM/Tasks/video_cluster/coral2013/Good_frames"))

# # Color_bob("OSTRS2013_S0073T0013_GOPR0651.mp4.jpg")
# # Color_bob("OSTRS2013_S0036T0014_GOPR0347.mp4.jpg")
# Color_bob("OSTRS2013_S0034T0014_GOPR0241.mp4.jpg")
# # Color_bob("OSTRS2013_S0004T0014_GOPR0606.mp4.jpg")
# Color_bob("OSTRS2013_S0036T0014_GOPR0347.mp4.jpg")
# # Color_bob("OSTRS2013_S0012T0014_GOPR0616.mp4.jpg")
# # Color_bob("OSTRS2013_S0004T0014_GOPR0616.mp4.jpg")
# # Color_bob("OSTRS2013_S0009T0014_GOPR0617.mp4.jpg")
# # Color_bob("OSTRS2013_S0004T0014_GOPR0610.mp4.jpg")
# Color_bob("OSTRS2013_S0004T0014_GOPR0608.mp4.jpg")

# for jpg_name in jpg_names:

#   Color_bob(jpg_name)

def Features( video_file, W, H, video_dir, skip, to_dir ):

  img = Mean_frame( video_file, W, H, video_dir, skip )
  jpg_name = video_file + ".jpg"
  Color_bob( img, to_dir, jpg_name )

# video_file = "STRS2013_S0036T0014_GOPR0347.mp4"
# W = 192
# H = 108
# video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video2013"
# skip = 5
# to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2013/Clear/features"

# Features( video_file, W, H, video_dir, skip, to_dir )












