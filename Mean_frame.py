import cv2
import numpy as np
from decimal import *
getcontext().prec = 6
import math
import os
import cPickle as pickle

def Mean_frame( video_file, video_dir, skip, data, Hgp, Wgp ):

  # frame_o = np.zeros((Wgp, Hgp, 3))

  os.chdir(video_dir)
  vidcap = cv2.VideoCapture(video_file)
  frame_rate = int(round(vidcap.get(5)))

  success, frame = vidcap.read()

  f = 0
  count = 2

  while success:

    success, frame = vidcap.read()

    if success == True:

      # frame = Remove_trap( frame )
      # frame = Resize_frame( frame, W, H )

      if f >= frame_rate*skip and f % 1 == 0:

        if f == frame_rate*skip and f % 1 == 0:

          frame_o = frame

        else:

          frame_o = cv2.addWeighted(frame_o, Decimal(1) - (Decimal(1)/Decimal(count)),
                    frame, Decimal(1)/Decimal(count), 0)

          count = count + 1

    f = f + 1

  if data == 1:

    frame_o = frame_o.flatten("C").copy()
    frame_o = np.array(frame_o)
    frame_o = np.clip(frame_o, 0, 255)
    frame_o = json.loads(json.dumps(frame_o.tolist()))

  return frame_o

