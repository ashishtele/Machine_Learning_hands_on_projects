
rm(list=ls())

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")


# Black and white image
library(EBImage)
f <- system.file("images", "sample.png",package = "EBImage")
img <- readImage(f)
display(img)


f_c <- system.file("images", "sample-color.png",package = "EBImage")
imgc <- readImage(f_c)
display(imgc)
nuc <- readImage(system.file("images", "nuclei.tif",package = "EBImage"))
display(nuc)

# writing/saving the image 
writeImage(imgc, 'imgc.jpeg', quality = 85)

# class of image
# multidimensional arrays with pixel intensities
class(imgc)

print(img)
############################# brightness control
img_br <- img + 0.2
display(img_br)

############################# contrast control 
img_contst <- img*3
display(img_contst)

# ########################### gamma correction parameter
img_gamma <- (img+0.2)^3
display(img_gamma)

############################# transpose
img_t <- t(img)
display(img_t)

############################# threshold 
img_thr <- img>0.5
display(img_thr)

############################# image crop
img_cr <- img[299:376, 224:301]
display(img_cr)

############################ multiple frames
imgcomb <- combine(img, img*2,img*3, img*4)
display(imgcomb)

print(imgc)
# color management
# Grayscale image with 3 frames
colorMode(imgc) <- Grayscale
display(imgc)
colorMode(imgc) <- Color

# image filtering

flo <- makeBrush(21, shape = 'disc', step = FALSE)^2
flo < flo/sum(flo)

imgflo <- filter2(imgc,flo)
display(imgflo)

fhi <- matrix(1, nc=3, nr=3)
fhi[2,2] = -8
imgfhi <- filter2(imgc,fhi)
display(imgfhi)

# Morphological oprations
# binary image

ei <- readImage(system.file('images','shapes.png',package = 'EBImage'))
ei <- ei[110:512, 1:130]
display(ei)
kern <- makeBrush(5, shape = 'diamond')
eierode <- erode(ei, kern)
display(eierode)
display(dilate(ei, kern))


# Segmentation
# bwlable for binary images and after thresholding 
nuct <- nuc[,,1]>0.2
display(nuct)
nuclable <- bwlabel(nuct)
# number of nuclei:
max(nuclable)

# Adaptive thresholding

nuct2 <- thresh(nuc[,,1],w=10,h=10,offset = 0.05)
display(nuct2)

kern <- makeBrush(5, shape='disc')
nuct2 <- dilate(erode(nuct2, kern), kern)
display(nuct2)
nuclabel12 <- bwlabel(nuct2)
max(nuclabel12)

# object manipulation
bucgray <- channel(nuc[,,1],'rgb')
display(bucgray)
display(paintObjects(nuclabel12,bucgray, col='#ff00ff'))
nuclabel13 <- fillHull(nuclabel12)
display(paintObjects(nuclabel13,bucgray, col='#ff00ff'))

################# Image processing ###############################

cel <- readImage(system.file("images", "cells.tif",package = "EBImage"))
display(cel)
img <- rgbImage(green=1.5*cel, blue=nuc)
display(img)

