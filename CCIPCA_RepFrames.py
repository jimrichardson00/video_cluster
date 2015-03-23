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
import gzip

def CCIPCA_RepFrames( RepFrames_new, n_components, rerun, year, W, H ):

  # RepFrames_new = numpy.array(RepFrames_new)
  n_components = int(n_components)
  rerun = int(rerun)

  ccipca = CCIPCA(n_components = n_components)

  if rerun == 0:

    # n_components = numpy.load("n_components.txt")  
    iteration = int(float(numpy.loadtxt("iteration" + year + ".txt"))) 
    amnesic = int(float(numpy.loadtxt("amnesic" + year + ".txt")))
    copy = int(float(numpy.loadtxt("copy" + year + ".txt")))
    mean_ = numpy.loadtxt("mean_" + year + ".txt")  
    components_ = numpy.loadtxt("components_" + year + ".txt")  
    components_ = numpy.transpose(components_)
    c0 = components_.shape[0]
    c1 = components_.shape[1]
    zeros = numpy.zeros((n_components, c1))
    zeros[0:c0, 0:c1] = components_
    components_ = zeros

    # ccipca.n_components = n_components  
    ccipca.iteration = iteration 
    ccipca.amnesic = amnesic
    ccipca.copy = copy  
    ccipca.mean_ = mean_  
    ccipca.components_ = components_

  ccipca = ccipca.fit(RepFrames_new)

  ccipca_dict = {}

  n_components = ccipca.n_components
  n_components = numpy.array(n_components).reshape(1, )
  print 'n_components'
  numpy.savetxt('n_components' + year + '.txt', n_components)

  iteration = ccipca.iteration
  iteration = numpy.array(iteration).reshape(1, )
  print 'iteration'
  numpy.savetxt('iteration' + year + '.txt', iteration)

  amnesic = ccipca.amnesic
  amnesic = numpy.array(amnesic).reshape(1, )
  print 'amnesic'
  numpy.savetxt('amnesic' + year + '.txt', amnesic)

  copy = ccipca.copy
  copy = numpy.array(copy).reshape(1, )
  print 'copy'
  numpy.savetxt('copy' + year + '.txt', copy)

  mean_ = ccipca.mean_
  mean_ = numpy.array(mean_).reshape(1, 3*W*H)
  mean_ = numpy.transpose(mean_)
  print 'mean_'
  numpy.savetxt('mean_' + year + '.txt', mean_)

  components_ = ccipca.components_
  components_ = numpy.array(components_)
  components_ = numpy.transpose(components_)
  print 'components_'
  numpy.savetxt('components_' + year + '.txt', components_)

# --------------------------------------------------

  # n_components = json.loads(json.dumps(n_components.tolist()))
  # ccipca_dict["n_components"] = n_components

  # iteration = json.loads(json.dumps(iteration.tolist()))
  # ccipca_dict["iteration"] = iteration

  # amnesic = json.loads(json.dumps(amnesic.tolist()))
  # ccipca_dict["amnesic"] = amnesic

  # copy = json.loads(json.dumps(copy.tolist()))
  # ccipca_dict["copy"] = copy

  # mean_ = json.loads(json.dumps(mean_.tolist()))
  # ccipca_dict["mean_"] = mean_ 

  # components_ = json.loads(json.dumps(components_.tolist()))
  # ccipca_dict["components_"] = components_

  # prx =  ccipca.transform(RepFrames_new)
  # prx = numpy.array(prx)
  # prx = json.loads(json.dumps(prx.tolist()))
  # ccipca_dict["prx"] = prx

  # ccipca_dict = numpy.array(ccipca_dict)
  # print 'ccipca_dict'
  # numpy.savetxt('ccipca_dict' + year + '', ccipca_dict)

  # return ccipca_dict

