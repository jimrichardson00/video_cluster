#!/usr/bin/env python

"""eigenface
finds the first k eigenfaces using pca and inc pca
the results are displayed side by side for visual
comparison
"""
import cv2
from sklearn.decomposition import PCA
from pyIPCA import CCIPCA
from os.path import join, exists
import urllib
import zipfile
import sys
import os
from decimal import *
import math
import numpy

video_dir = '/home/jim/Desktop/video_cluster/video'
data_dir = '/home/jim/Desktop/video_cluster/data'
frames_dir = '/home/jim/Desktop/video_cluster/frames'

def CCIPCA_RepFrames( RepFrames_new, n_components ):

  n_components = int(n_components)

  ccipca = CCIPCA(n_components = n_components)

  ccipca = ccipca.fit(RepFrames_new)

  ccipca_dict = {}

  n_components = ccipca.n_components
  n_components = numpy.array(n_components)
  n_components = json.loads(json.dumps(n_components.tolist()))
  ccipca_dict["n_components"] = n_components

  iteration = ccipca.iteration
  iteration = numpy.array(iteration)
  iteration = json.loads(json.dumps(iteration.tolist()))
  ccipca_dict["iteration"] = iteration

  amnesic = ccipca.amnesic
  amnesic = numpy.array(amnesic)
  amnesic = json.loads(json.dumps(amnesic.tolist()))
  ccipca_dict["amnesic"] = amnesic

  copy = ccipca.copy
  copy = numpy.array(copy)
  copy = json.loads(json.dumps(copy.tolist()))
  ccipca_dict["copy"] = copy

  mean_ = ccipca.mean_
  mean_ = numpy.array(mean_)
  mean_ = json.loads(json.dumps(mean_.tolist()))
  ccipca_dict["mean_"] = mean_ 

  components_ = ccipca.components_
  components_ = numpy.array(components_)
  components_ = json.loads(json.dumps(components_.tolist()))
  ccipca_dict["components_"] = components_

  return ccipca_dict
