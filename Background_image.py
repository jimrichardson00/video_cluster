import numpy as np
import cv2
from decimal import *
getcontext().prec = 6
import math

H = 108*5
W = 192*5

skip = 5

video_file = "STRS2013_S0041T0013_GOPR0627.mp4"
video_file = "STRS2013_S0036T0014_GOPR0347.mp4"
vidcap = cv2.VideoCapture(video_file)
frame_rate = int(round(vidcap.get(5)))

f = 0

success, img = vidcap.read()

while success:

  success, img = vidcap.read()

  if success == True:

    if f == 300:

        success, img = vidcap.read()

        height, width, depth = img.shape

        x1 = int(math.floor(Decimal(0.26041666666)*Decimal(width)))
        x2 = int(math.floor(Decimal(0.73958333334)*Decimal(width)))
        y1 = 0
        y2 = int(math.floor(Decimal(0.65555555555)*Decimal(height)))
        img = img[y1:y2, x1:x2]

        img = cv2.resize(img, (W, H))

        Z = img.reshape((-1, 3))

        # convert to np.float32
        Z = np.float32(Z)

        # define criteria, number of clusters(K) and apply kmeans()
        criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
        K = 50
        ret, label, center = cv2.kmeans(Z, K, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)

        # Now convert back into uint8, and make original image
        center = np.uint8(center)
        res = center[label.flatten()]
        res2 = res.reshape((img.shape))

        cv2.imshow('draw', res2)
        k = cv2.waitKey(30) & 0xff
        if k == 27:
          break

  f = f + 1

# cv2.waitKey(0)
# cv2.destroyAllWindows()

