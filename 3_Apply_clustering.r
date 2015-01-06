require(dendextend)
require(dendroextras)
require(plot3D)
require(rgl)
require(plyr)
require(stringr)
require(NbClust)
require(modeest)
require(scatterplot3d)
require(beepr)
require(rPython)
require(MASS)
require(Hmisc)
require(beepr)
require(plyr)
require(parallel)
require(doParallel)
require(foreach)

n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1
n_cores
n_components = 100
W = 192
H = 108
frame_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame"
video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video"
master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster"

posVar <- function(data) {
	out <- lapply(data, function(x) length(unique(x)))
	want <- which(out > 1)
	unlist(want)
}

CCIPCA_video <- function(video_file, n_components, W, H, fast) {

	require(rPython)

	setwd(master_dir)
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

	} else if(fast == 1) {

		rep_idx <- 1

	}

	if(fast == 0 | fast == 1){
		return(framesData[rep_idx,])
	} else if (fast == 3){
		return(framesData)
	}
}

Video_var <- function(video_file, W, H, skip = 6) {

	print(video_file)

	setwd(master_dir)
	python.load("Video_var.py")
	Video_var <- python.call("Video_var", video_file, W, H, skip)

	return(Video_var)
}


video_file = "STRS2013_S087T014_GOPR0411.MP4"
Video_mod <- function(video_file, W, H, skip = 6) {

	fast = 0

	setwd(master_dir)
	python.load("CCIPCA_video.py")
	output <- python.call("CCIPCA_video", video_file, n_components, W, H, skip, fast)

	framesData <- matrix(unlist(output[[1]]), ncol = 3*W*H, byrow = TRUE)
	components_ <- matrix(unlist(output[[2]]), ncol = 3*W*H, byrow = TRUE)

	prx <- t(components_ %*% t(framesData))

	for(j in seq(1, ncol(prx), 1)) {
		mode = density(prx[, j])$x[which.max(density(prx[, j])$y)]
		prx[, j] <- prx[, j] - mode
	}

	return(mean(apply(prx, MARGIN = 2, FUN = function(x) mean(sum(x^2)))))
}

CCIPCA_RepFrames <- function(RepFrames_new, n_components, rerun) {

	setwd(master_dir)

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

SetTrap = "STRS2013_S005T014"
skip = 6
n_components = 700
W = 192
H = 108
CCIPCA_SetTrap <- function(SetTrap, W = 192, H = 108, skip = 6, video_dir = video_dir) {

	print(paste("Starting: ", SetTrap, sep = ""))

	setwd(master_dir)
	python.load("CCIPCA_SetTrap.py")
	output <- python.call("CCIPCA_SetTrap", SetTrap, W, H, skip, video_dir)
	prx <- matrix(unlist(output[[1]]), ncol = sqrt(length(unlist(output[[1]]))), byrow = TRUE)
	video_files_frames <- output[[2]]

	min = min(as.numeric(str_match(video_files_frames, "(.+)_GOPR0(.+)_F(.+)")[, 3]))

	cols <- as.numeric(str_match(video_files_frames, "(.+)_GOPR0(.+)_F(.+)")[, 3]) - min + 1

	video_files <- na.omit(str_match(video_files_frames, paste("(", SetTrap, ".+)", "_F.+", sep = ""))[, 2])

	video_files_mod <- unlist(lapply(sort(unique(video_files)), FUN = function(video_file) diff_from_mode(prx[video_files == video_file, ])))

	setwd(master_dir)
	png(paste(SetTrap, ".png", sep = ""))
	plot(prx[, 1:2], col = cols, main = SetTrap)
	legend("topright", fill = sort(unique(cols)),
	 legend = paste(unique(sort(cols)) + min - 1, " ", 
			"Mod-", formatC(video_files_mod, flag = "0", width = 9, format = "d"), sep = ""))
	dev.off()

	m <- as.matrix(data.frame(SetTrap = rep(SetTrap, length(unique(video_files))), video_files = sort(unique(video_files)), video_files_mod = video_files_mod), ncol = 3, drop = FALSE)

	print(paste("Finished: ", SetTrap, sep = ""))

	return(m)

}

Output_image <- function(name, data, dest_dir, W, H) {

	setwd(master_dir)

	data <- as.vector(data)
	N <- length(data)/3

	blu = matrix(data[seq(1, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	gre = matrix(data[seq(2, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	red = matrix(data[seq(3, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	
	python.load("Output_image.py")
	python.call("Output_image", name, red, gre, blu, dest_dir, W, H)
}

diff_from_mode <- function(prx) {
	for(j in seq(1, ncol(prx), 1)) {
		modej = density(prx[, j])$x[which.max(density(prx[, j])$y)]
		prx[, j] <- prx[, j] - modej
	}
	return(mean(apply(prx, MARGIN = 2, FUN = function(x) mean(sum(x^2)))))
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
master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster"
video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/vide"
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
	SetTraps_cur = ccipca[["SetTraps"]]

} else {
	
	RepFrames_cur <- data.frame()
	video_files_cur <- vector()
	SetTraps_cur <- vector()

}

# list of videos and SetTraps in video folder
setwd(video_dir)
video_files <- list.files()
video_files
SetTraps <- unique(str_match(sort(list.files(video_dir)), "(.+)_GOPR")[, 2])
SetTraps

# defines new videos to be added
video_files_new <- video_files[!(video_files %in% video_files_cur)]
video_files_new
SetTraps_new <- SetTraps[!(SetTraps %in% SetTraps_cur)]
SetTraps_new

# calculates new rep frames
video_file <- video_files_new[124]
video_file

# calculates difference from mode for each video file
# Video_var_v <- unlist(mclapply(video_files, FUN = function(video_file) Video_var(video_file = video_file, W = W, H = H, skip = 6), mc.cores = n_cores))
Video_var_L <- mclapply(SetTraps, FUN = function(SetTrap) CCIPCA_SetTrap(SetTrap = SetTrap, W = W, H = H, skip = 6, video_dir = video_dir), mc.cores = n_cores)
Video_var_L

Video_var_df <- data.frame(video_files = video_files, SetTrap = str_match(video_files, "(.+)_GOPR(.+)")[, 2], Video_var = Video_var_v)
Video_var_min <- ddply(Video_var_df, .(SetTrap), summarise, Min_video_var = min(Video_var), Min_video_file = video_files[which.min(Video_var)])
Video_var_min

Video_var_df$Video_var <- round(Video_var_df$Video_var)
Video_var_df$Video_var <- as.character(Video_var_df$Video_var)
Video_var_df$Video_var <- formatC(Video_var_df$Video_var, flag = "0", width = 10, format = "d")
Video_var_df <- Video_var_df[order(Video_var_df$Video_var), ]
head(Video_var_df)

# outputs video to picture
foreach(i = seq(1, nrow(Video_var_df), 1)) %dopar% {

	video_file = Video_var_df$video_files[i]
	Video_var = Video_var_df$Video_var[i]

	name = paste(Video_var, "_", video_file, sep = "")

	print(name)

	data = CCIPCA_video(video_file = video_file, n_components = 10, W = W, H = H, fast = 1)

	Output_image(name = name,
	 data = data, 
	 dest_dir = paste(master_dir, "/video_var", sep = "") 
	 W = W, 
	 H = H)
}

beep()

video_files_new <- as.character(Video_var_min$Min_video_file)
video_files_new

i = 2
# cacluates the new rep frames matrix
if(length(video_files_new) > 0) {
	RepFrame_new_v <- unlist(mclapply(video_files_new, FUN = function(video_file) CCIPCA_video(video_file = video_file, n_components = 10, W = W, H = H, fast = 0), mc.cores = n_cores))
	RepFrames_new <- matrix(RepFrame_new_v, ncol = 3*W*H, byrow = TRUE) 
}

beep()

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
setwd(master_dir)
save(ccipca, file = 'ccipca.RData')

# -------------------------------------------------------
# -------------------------------------------------------
# plots plot of first 2, and first 3 principal components

# write ccipca list to RData
setwd(master_dir)
load(file = 'ccipca.RData')

# first two components
plot(ccipca[["prx"]][, 1], ccipca[["prx"]][, 2])

# install rgl library
plot3d(ccipca[["prx"]][, 1:3])

# pca matrix, first two principal components
prx <- ccipca[["prx"]]
prx <- as.data.frame(prx)
row.names(prx) <- ccipca[["video_files_cur"]]

NbClustN <- NbClust(data = prx[, posVar(prx)], min.nc = 2, max.nc = nrow(prx) - 2,
 method = 'kmeans')
NbClust$Best.nc[1]
NbClust

prx[, 1:3]

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
text(km$centers[, 1:2], labels = sort(unique(clusters)), pos = 3)

sort(unique(clusters))
cbind(prx[, 1:2], clusters)
km$centers

clusters <- km$cluster

# ---------------------------------------------------
# copies frames into cluster folders, and into rep_frames folder

# deletes rep_frames folder, then creates it (aviods overlap)
frame_dir = "/home/jim/Desktop/video_cluster/frames"
master_dir = "/home/jim/Desktop/video_cluster"
system(paste('rm -r ', frame_dir, sep = ""))
system(paste('mkdir ', frame_dir, sep = ""))

# cycles through clusters
for(cl in unique(clusters)){

	print(cl)

	# deletes cluster folder, then creates it (aviods overlap)
	system(paste("rm -r ", frame_dir, "/", cl, sep = ""))
	system(paste("mkdir ", frame_dir, "/", cl, sep = ""))

	names_cl <- ccipca[["video_files_cur"]][clusters == cl]

	for(i in seq(1, sum(clusters == cl), 1)){

		name = names_cl[i]
		data = ccipca[["RepFrames_cur"]][clusters == cl, ][i, ]

		dest_dir = frames_dir
		Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

		dest_dir = paste(frame_dir, "/", cl, sep = "")
		Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

	}
}

plot(prx[, 1:2], col = clusters)

beep()

# ----------------------------------------
# Output Eigentraps

setwd(master_dir)
load("ccipca.RData")

# number of prinicpal components to output
n_pc <- 5

pc <- 2
for(pc in seq(1, n_pc, 1)){

	name <- paste("prcomp", formatC(pc, flag = "0", width = 2), sep = "")

	data <- ccipca[["components_"]][pc, ]
	data <- data*255/max(data)

	W = 192
	H = 108
	dest_dir = master_dir

	Output_image(name, data, dest_dir, W, H)

}

# --------------------------------------------------
# --------------------------------------------------
# for loop to calculate Video_mod for each Set Trap
# (this is now contained in CCIPCA_SetTrap)

setwd(video_dir)
SetTraps <- unique(str_match(sort(list.files(getwd())), "(.+)_GOPR")[, 2])

SetTrap = "STRS2013_S087T014"

n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1
n_components = 100
W = 192
H = 108
frame_dir = "/home/jim/Desktop/video_cluster/frame"
video_dir = "/home/jim/Desktop/video_cluster/video"
master_dir = "/home/jim/Desktop/video_cluster"

video_file = "STRS2013_S087T014_GOPR0411.MP4"

registerDoParallel(cores = n_cores)

Video_files_mod_df <- matrix(NA, nrow = length(list.files(video_dir)), ncol = 4)
for(SetTrap in SetTraps) {

	n_components = 300
	W = 192
	H = 108
	skip = 6

	print(SetTrap)

	setwd(video_dir)
	python.load("CCIPCA_SetTrap.py")
	output <- python.call("CCIPCA_SetTrap", SetTrap, n_components, W, H, skip)
	prx <- matrix(unlist(output[[1]]), ncol = sqrt(length(unlist(output[[1]]))), byrow = TRUE)
	video_files_frames <- output[[2]]

	min = min(as.numeric(str_match(video_files_frames, "(.+)_GOPR0(.+)_F(.+)")[, 3]))

	cols <- as.numeric(str_match(video_files_frames, "(.+)_GOPR0(.+)_F(.+)")[, 3]) - min + 1

	video_files <- na.omit(str_match(video_files_frames, paste("(", SetTrap, ".+)", "_F.+", sep = ""))[, 2])
	
	video_files_var <- unlist(lapply(paste(sort(unique(video_files)), ".MP4", sep = ""), FUN = function(video_file) Video_var(video_file, W = W, H = H, skip = 6)))

	video_files_mod <- unlist(lapply(sort(unique(video_files)), FUN = function(video_file) diff_from_mode(prx[video_files == video_file, ])))

	setwd(master_dir)
	png(paste(SetTrap, ".png", sep = ""))
	plot(prx[, 1:2], col = cols, main = SetTrap)
	legend("topright", fill = sort(unique(cols))
	, legend = paste(unique(sort(cols)) + min - 1, " ", 
			"Var-", formatC(video_files_var, flag = "0", width = 9, format = "d"), " ", 
			"Mod-", formatC(video_files_mod, flag = "0", width = 9, format = "d"), sep = ""))
	dev.off()

	m <- as.matrix(data.frame(SetTrap = rep(SetTrap, length(unique(video_files))), video_files = sort(unique(video_files)), video_var = video_files_var, video_files_mod = video_files_mod), ncol = 4, drop = FALSE)

	Video_files_mod_df[seq(row_ind, row_ind + length(unique(video_files)) - 1, 1), ] = m[, ]

	row_ind <- row_ind + length(unique(video_files))

	print(row_ind)
}

Video_files_mod_df

# ---------------------------------------
# animation of video, or settrap

ani <- function(){
	for(i in seq(1, nrow(prx))){
		plot(prx[i, 1], prx[i, 2], ylim = range(prx[, 2]), xlim = range(prx[, 1]), main = video_files_frames[i])
		points(pc1, pc2, col = "red")
	}
}

saveGIF(ani())

# ----------------------------------------
# output frames to jpeg with cluster names in filename

# for given framesData data set, writes each frame to jpeg file
# adds clustername to start of jpeg file to see which cluster it is in
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
