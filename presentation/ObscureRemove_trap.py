import cv2
from decimal import *
getcontext().prec = 6
import math
import numpy
import os
import cPickle as pickle

def Obscure_trap( frame ):

  height, width, depth = frame.shape

  x1 = int(round(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(round(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(round(Decimal(0.65555555555)*Decimal(height)))

  obscure = numpy.zeros((1080, 1920, 3))
  obscure[y1:y2, x1:x2] = frame[y1:y2, x1:x2]

  return obscure

def Remove_trap( frame ):

  height, width, depth = frame.shape

  x1 = int(round(Decimal(0.26041666666)*Decimal(width)))
  x2 = int(round(Decimal(0.73958333334)*Decimal(width)))
  y1 = 0
  y2 = int(round(Decimal(0.65555555555)*Decimal(height)))

  frame = frame[y1:y2, x1:x2]

  return frame

video_file = "STRS2013_S036T014_GOPR0347"

# obscure

frame = cv2.imread(video_file + ".jpg")
frame = Obscure_trap( frame )
cv2.imwrite(video_file + "_TrapObscure" + ".jpg", frame)

frame = cv2.imread(video_file + ".jpg")
frame = Remove_trap( frame )
cv2.imwrite(video_file + "_TrapRemoved" + ".jpg", frame)
