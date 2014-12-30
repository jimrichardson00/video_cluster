library(dendextend)
library(dendroextras)
library(plot3D)
library(rgl)
library(plyr)
library(stringr)
library(NbClust)
library(modeest)
library(scatterplot3d)
library(beepr)
library(rPython)
library(MASS)
library(Hmisc)
library(beepr)

n_components = 100
W = 192
H = 108
frames_dir = "/home/jim/Desktop/video_cluster/rep_frames"
video_dir = "/home/jim/Desktop/video_cluster/video"
master_dir = "/home/jim/Desktop/video_cluster"

posVar <- function(data) {
	out <- lapply(data, function(x) length(unique(x)))
	want <- which(out > 1)
	unlist(want)
}

extract_data <- function(DFO2013_SiiiTjjj_GP) {
	setwd("/home/jim/Desktop/video_cluster/")
	system(paste("python 2_Extract_data.py ", DFO2013_SiiiTjjj_GP, ".MP4", sep = ""))
}

CCIPCA_video <- function(video_file, n_components, W, H, fast) {

	setwd("/home/jim/Desktop/video_cluster/")
	python.load("CCIPCA_video.py")
	output <- python.call("CCIPCA_video", video_file, n_components, W, H, fast)

	framesData <- matrix(unlist(output[[1]]), ncol = 3*W*H, byrow = TRUE)

	if(fast == 0) {

		components_ <- matrix(unlist(output[[2]]), ncol = 3*W*H, byrow = TRUE)

		prx <- t(components_ %*% t(framesData))

		# optimal number of cluster
		NbClust <- NbClust(data = prx, 
			method = 'kmeans', min.nc = 2, max.nc = nrow(prx) - 2,
			index = 'ch')
		NbClust

		fit <- kmeans(prx, centers = NbClust$Best.nc[1])
		cl <- fit$cluster

		rep_idx <- seq(1, nrow(prx), by = 1)[cl == mfv(cl)][1]

	} else {
		rep_idx <- 1
	}
	return(t(framesData)[, rep_idx])
}

setwd("/home/jim/Desktop/video_cluster/video")
list.files()
video_file <- "STRS2013_S004T014_GOPR0606.MP4"
SetTrap <- str_match(video_file, "(.+)_GOPR(.+)")[, 2]
SetTrap

n_components = 20
W = 192
H = 108
3*W*H

CCIPCA_SetTrap <- function(SetTrap, n_components, W, H, fast) {

	setwd("/home/jim/Desktop/video_cluster/")
	python.load("CCIPCA_SetTrapVar.py")
	output <- python.call("CCIPCA_SetTrapVar", video_file, n_components, W, H)
	python.call("CCIPCA_SetTrapVar", video_file, n_components, W, H)
	output
	beep()

	framesData[[1]]
	components_[[1]]

	framesData <- matrix(unlist(output[[1]]), ncol = 3*W*H, byrow = TRUE)
	components_ <- matrix(unlist(output[[2]]), ncol = 3*W*H, byrow = TRUE)
	video_files_frames <- output[[3]]
	prx <- t(components_ %*% t(framesData))

	nrow(prx)
	ncol(prx)

	# optimal number of cluster
	# NbClust <- NbClust(data = prx[, 1:2], 
	# 	method = 'kmeans', min.nc = 2, max.nc = nrow(prx) - 2,
	# 	index = 'kl')
	# NbClust <- NbClust$Best.nc[1]
 	NbClust <- round(sqrt(nrow(prx)/2), 0)
 	NbClust

	fit <- kmeans(prx, centers = NbClust)
	clusters <- fit$cluster
	clusters

	plot(prx[, 1:2], col = clusters)
	points(fit$centers, pch = 16)
	text(fit$centers[, 1:2], paste(unique(clusters)), pos = 2)
	
	plot3d(prx[, 1:3])

	data = t(t(components_) %*% fit$centers[2, ])
	fit$centers
	ncol(components_)
	nrow(components_)
	
	unique(clusters)[min(fit$withinss) == fit$withinss]

	for(i in seq(1, length(video_files_frames), 1)) {		
		name = paste(clusters[i], "_", video_files_frames[i], sep = "")
		data = framesData[i, ]
		dest_dir = frames_dir
		W = 192
		H = 108
		Output_image(name, data, dest_dir, W, H)

		name = paste(clusters[i], sep = "")
		data = t(t(components_) %*% fit$centers[clusters[i], ])
		dest_dir = frames_dir
		W = 192
		H = 108
		Output_image(name, data, dest_dir, W, H)
	}

	rep_idx <- seq(1, nrow(prx), by = 1)[clusters == mfv(clusters)][1]
	rep_idx

	return(t(framesData)[, rep_idx])
}

CCIPCA_RepFrames <- function(RepFrames_new, n_components, rerun) {

	setwd("/home/jim/Desktop/video_cluster")

	RepFrames_new <- matrix(unlist(RepFrames_new[1:nrow(RepFrames_new),1:ncol(RepFrames_new)]), nrow = nrow(RepFrames_new), byrow = FALSE)

	if(file.exists("ccipca.RData") == TRUE & rerun == FALSE) {

		python.assign("n_components", n_components)  
		python.assign("iteration", iteration) 
		python.assign("amnesic", amnesic)
		python.assign("copy", copy)  
		python.assign("mean_", mean_)  
		python.assign("components_", components_)  
		python.assign("RepFrames_cur", RepFrames_cur)  
		python.assign("video_files_cur", video_files_cur)  

	}

	python.assign("n_components", n_components)  
	python.load("CCIPCA_RepFrames.py")
	ccipca <- python.call("CCIPCA_RepFrames", RepFrames_new, n_components)
	return(ccipca)

}

Output_image <- function(name, data, dest_dir, W, H) {

	setwd("/home/jim/Desktop/video_cluster")

	data <- as.vector(data)
	N <- length(data)/3

	blu = matrix(data[seq(1, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	gre = matrix(data[seq(2, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	red = matrix(data[seq(3, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	
	python.load("Output_image.py")
	python.call("Output_image", name, red, gre, blu, dest_dir, W, H)
}

# ----------------------------------------------------------------
# copies videos into one folder with labels

from_dirs = c('/home/jim/Desktop/video_cluster/october 2013 video')
from_idxs = c("STRS2013")
to_dir =  '/home/jim/Desktop/video_cluster/video2'

for(i in seq(1, length(from_dirs),1 )) {
	from_dir = from_dirs[i]
	from_idx = from_idxs[i]
	python.load("1_Rename_video.py")
	python.call("Rename_video", from_idxs, from_dir, to_dir)
}

# ---------------------------------------------------------------
# overlays frame from each video to see the max non trap area

W = 192
H = 108
video_dir = "/home/jim/Desktop/video_cluster/video"
master_dir = "/home/jim/Desktop/video_cluster"
setwd(master_dir)
python.load("Add_weighted.py")
python.call("Add_weighted", video_dir, master_dir, W, H)

# ----------------------------------------------------------------
# extracts data from each video and pulls out representative frame

# checks if there is a current ccipca file
# and loads variables if there is:
setwd(master_dir)
if(file.exists("ccipca.RData") == TRUE) {

	load("ccipca.RData")

	n_components = ccipca[["n_components"]]
	iteration = ccipca[["iteration"]] 
	amnesic = ccipca[["amnesic"]] 
	copy = ccipca[["copy"]] 
	mean_ = ccipca[["mean_"]] 
	components_ = ccipca[["components_"]] 
	RepFrames_cur = ccipca[["RepFrames_cur"]] 
	video_files_cur = ccipca[["video_files_cur"]]

} else {
	
	RepFrames_cur <- data.frame()
	video_files_cur <- vector()

}

# list of videos in video folder
setwd("/home/jim/Desktop/video_cluster/video")
video_files <- list.files()
video_files

# defines new videos to be added
video_files_new <- video_files[!(video_files %in% video_files_cur)]
video_files_new

# calculates new rep frames
video_file <- video_files_new[124]
video_file

# cacluates the new rep frames matrix
RepFrames_new <- matrix(NA, nrow = length(video_files_new), ncol = 3*H*W)
if(nrow(RepFrames_new) > 0) {
	for(i in seq(1, length(video_files_new), 1)) {
		video_file <- video_files_new[i]
		print(paste(video_file, " - ", i, sep = ""))
		RepFrame <- CCIPCA_video(video_file = video_file, n_components = 10, W = W, H = H, fast = 0)
		RepFrames_new[i, ] <- RepFrame
	}
}

beep()

nrow(RepFrames_new)
ncol(RepFrames_new)

write.csv(RepFrames_new, "RepFrames_new.csv")

# add new points to current pca
n_components = 100
ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = 100, rerun = TRUE)

# updated values
n_components <- ccipca_dict[["n_components"]]  
iteration <- ccipca_dict[["iteration"]]  
amnesic <- ccipca_dict[["amnesic"]] 
copy <- ccipca_dict[["copy"]]   
mean_ <- ccipca_dict[["mean_"]]
components_ <- matrix(unlist(ccipca_dict[["components_"]]), ncol = 3*W*H, byrow = TRUE)
if(nrow(components_) < n_components) {
	n_zeroes <- n_components - nrow(components_)
	zeroes <- matrix(0, nrow = n_zeroes, ncol = 3*H*W)
	components_ <- rbind(components_, zeroes)
}
RepFrames_cur <- rbind(RepFrames_cur, RepFrames_new)
video_files_cur <- c(video_files_cur, video_files_new)
prx <- t(components_ %*% t(RepFrames_cur))

# update ccipca list with new values
ccipca <- list()
ccipca[["n_components"]] <- n_components  
ccipca[["iteration"]] <- iteration 
ccipca[["amnesic"]] <- amnesic
ccipca[["copy"]] <- copy  
ccipca[["mean_"]] <- mean_  
ccipca[["components_"]] <- components_  
ccipca[["RepFrames_cur"]] <- RepFrames_cur  
ccipca[["video_files_cur"]] <- video_files_cur  
ccipca[["prx"]] <- prx

# write ccipca list to RData
setwd("/home/jim/Desktop/video_cluster")
save(ccipca, file = 'ccipca.RData')

# -------------------------------------------------------
# -------------------------------------------------------

# write ccipca list to RData
setwd("/home/jim/Desktop/video_cluster")
load(file = 'ccipca.RData')

# first two components
plot(ccipca[["prx"]][, 1], ccipca[["prx"]][, 2])

# install rgl library
plot3d(ccipca[["prx"]][, 1:3])

# pca matrix, first two principal components
prx <- ccipca[["prx"]]
prx <- as.data.frame(prx)
row.names(prx) <- ccipca[["video_files_cur"]]

NbClust <- NbClust(data = prx[, posVar(prx)], min.nc = 2, max.nc = nrow(prx) - 2,
 method = 'kmeans')
NbClust$Best.nc[1]
NbClust

# ----------------------------------------------------
# applies hclust clustering

k = 5

prx <- ccipca[["prx"]]
prx <- as.data.frame(prx)
row.names(prx) <- ccipca[["video_files_cur"]]

# define distance and number of clusters
dist <- dist(prx[, c(1, 2)], method = 'euclidean')

# apply clustering algorithm
fit <- hclust(dist, method = "average") 

# pull out cluster names
sl <- slice(fit, k = k)
sl <- as.matrix(sl)
sl <- as.data.frame(sl)
sl$rn <- row.names(sl)
sl <- sl[order(sl$rn),]

# define cluster names
clusters <- sl$V1
clusters

# create png of dendogram
png("dendrogram.png", height=H, width=W)
fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE)
plot(fit_col)
rect.dendrogram(fit_col, k = k, border = "red")
dev.off()

plot(prx[, c(1, 2)], col = clusters)

# --------------------------------------------------
# applies kmeans clustering

prx <- ccipca[["prx"]]
prx <- as.data.frame(prx)
row.names(prx) <- ccipca[["video_files_cur"]]

# 2d
# define distance and number of clusters
k <- 10
# apply clustering algorithm
km <- kmeans(prx[, c(1, 2)], centers = k) 
clusters <- km$cluster
plot(prx[, 1], prx[, 2], col = km$cluster)
points(km$centers[, c(1, 2)], pch = 16)

clusters <- km$cluster

# n_compenents d
k <- 10
km <- kmeans(ccipca[["prx"]], centers = k) 
plot(ccipca[["prx"]][, 1], ccipca[["prx"]][, 2], col = km$cluster)
points(km$centers[, c(1, 2)], pch = 16)

clusters <- km$cluster

# ---------------------------------------------------
# copies frames into cluster folders, and into rep_frames folder

# deletes rep_frames folder, then creates it (aviods overlap)
frame_dir = "/home/jim/Desktop/video_cluster/frames"
master_dir = "/home/jim/Desktop/video_cluster"
system(paste('rm -r ', master_dir, '/', 'rep_frames', sep = ""))
system(paste('mkdir ', master_dir, '/', 'rep_frames', sep = ""))

# cycles through clusters
for(cl in unique(clusters)){

	print(cl)

	# deletes cluster folder, then creates it (aviods overlap)
	system(paste('rm -r ', master_dir, '/rep_frames/', cl, sep = ""))
	system(paste('mkdir ', master_dir, '/rep_frames/', cl, sep = ""))

	names_cl <- ccipca[["video_files_cur"]][clusters == cl]

	for(i in seq(1, sum(clusters == cl), 1)){

		name = names_cl[i]
		data = ccipca[["RepFrames_cur"]][clusters == cl, ][i, ]

		dest_dir = "/home/jim/Desktop/video_cluster/rep_frames"
		Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

		dest_dir = paste("/home/jim/Desktop/video_cluster/rep_frames/", cl, sep = "")
		Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

	}
}

clusters
plot(prx[, 1:2], col = clusters)


beep()

# ----------------------------------------
# eigen traps

setwd('/home/jim/Desktop/video_cluster/')
load("ccipca.RData")

n_pc <- 5

pc <- 2
for(pc in seq(1, n_pc, 1)){

	name <- paste("prcomp", formatC(pc, flag = "0", width = 2), sep = "")

	data <- ccipca[["components_"]][pc, ]
	data <- data*255/max(data)

	dest_dir <- "/home/jim/Desktop/video_cluster"

	W <- 192
	H <- 108

	Output_image(name, data, dest_dir, W, H)

}