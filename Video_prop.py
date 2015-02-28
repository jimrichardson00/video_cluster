import numpy as np
import cv2
from matplotlib import pyplot as plt
import os
import math

video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video2013"
video_file = "STRS2013_S0004T0014_GOPR0615.mp4"

os.chdir(video_dir)
vidcap = cv2.VideoCapture(video_file)

print vidcap.get(1)
print vidcap.get(2)
print vidcap.get(3)
print vidcap.get(4)
print vidcap.get(5)
print vidcap.get(6)
print vidcap.get(7)
print vidcap.get(8)
print vidcap.get(9)
print vidcap.get(10)
print vidcap.get(11)
print vidcap.get(12)
print vidcap.get(13)
print vidcap.get(14)
print vidcap.get(15)