import cv2
import numpy
import cv

# pc1
image_pc1_blu = numpy.loadtxt("image_pc1_blu.txt")
image_pc1_gre = numpy.loadtxt("image_pc1_gre.txt")
image_pc1_red = numpy.loadtxt("image_pc1_red.txt")

image_pc1 = numpy.zeros((108, 192, 3), numpy.uint8)

image_pc1[:, :, 0] = image_pc1_blu
image_pc1[:, :, 1] = image_pc1_gre
image_pc1[:, :, 2] = image_pc1_red

cv2.imwrite("image_pc1.jpg", image_pc1)

# pc2
image_pc2_blu = numpy.loadtxt("image_pc2_blu.txt")
image_pc2_gre = numpy.loadtxt("image_pc2_gre.txt")
image_pc2_red = numpy.loadtxt("image_pc2_red.txt")

image_pc2 = numpy.zeros((108, 192, 3), numpy.uint8)

image_pc2[:, :, 0] = image_pc2_blu
image_pc2[:, :, 1] = image_pc2_gre
image_pc2[:, :, 2] = image_pc2_red

cv2.imwrite("image_pc2.jpg", image_pc2)

# pc3
image_pc3_blu = numpy.loadtxt("image_pc3_blu.txt")
image_pc3_gre = numpy.loadtxt("image_pc3_gre.txt")
image_pc3_red = numpy.loadtxt("image_pc3_red.txt")

image_pc3 = numpy.zeros((108, 192, 3), numpy.uint8)

image_pc3[:, :, 0] = image_pc3_blu
image_pc3[:, :, 1] = image_pc3_gre
image_pc3[:, :, 2] = image_pc3_red

cv2.imwrite("image_pc3.jpg", image_pc3)
