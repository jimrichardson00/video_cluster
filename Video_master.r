require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1

require(doParallel)
registerDoParallel(cores = n_cores)

H = 230
W = 177
Hgp = 1080
Wgp = 1920
skip = 6
year = ""
# year = "STRS2013"
# year = "STRS2014"
# year = "SKBO2014"
# mac = TRUE
mac = FALSE
if(mac == TRUE) {
	master_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster"
	} else {
	master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster"
}
frame_dir = paste(master_dir, "/frame", year, sep = "")
coral_dir = paste(master_dir, "/coral", year, sep = "")
video_dir = paste(master_dir, "/video", year, sep = "")
audio_dir = paste(master_dir, "/audio", year, sep = "")
proje_dir = paste(master_dir, "/proje", year, sep = "")
clust_dir = paste(master_dir, "/clust", year, sep = "")

# --------------------------------------------
# sources functions needed
setwd(master_dir)
source("Video_functions.r")

# --------------------------------------------
# copies videos into one folder with labels

from_dirs = c("/home/jim/Desktop/2014 SKB video")
from_idxs = c("SKBO2014")
to_dir =  video_dir
# source("Video_prepfile.r")

# --------------------------------------------
# applies ccipca on video data and saves it to rdata

rerun = 0
fast = 0
setwd(master_dir)
source("Video_ccipca.r")

# ---------------------------------------------------
# copies frames into cluster folders, and into rep_frames folder

k = 40
setwd(master_dir)
source("Video_cluster.r")

# ----------------------------------------
# train video data on existing setwd

setwd(master_dir)
N = 10

setwd(master_dir)
load(paste("ccipca_videoSTRS2013.RData", sep = ""))
video_files_cur <- ccipca[["video_files_cur"]]

Clear <- character(0)
require(stringr)
Clear <- na.omit(str_match(list.files(paste(clust_dir, "/Clear", sep = "")), "(.+)\\.jpg")[, 2])
Clear
ImageClarity <- ifelse(video_files_cur %in% Clear, "Clear", "Cloudy")
ImageClarity

Biotic <- character(0)
list.files(paste(clust_dir, "/Clear/IfBiotic", sep = ""))
IfBiotic <- na.omit(str_match(list.files(paste(clust_dir, "/Clear/IfBiotic", sep = "")), "(.+)\\.jpg")[, 2])
IfBiotic <- ifelse(video_files_cur %in% Biotic, "1", "0")
IfBiotic

Outputs <- c("ImageClarity")

source("Video_training_data.r")
source("Video_training.r")

# ----------------------------------------
# apply clear classifier to 2014 data

setwd(master_dir)
load(paste("ccipca_videoSKBO2014.RData", sep = ""))
# load(paste("ccipca_videoSTRS2014.RData", sep = ""))
RepFrames_cur <- ccipca[["RepFrames_cur"]]
video_files_cur <- ccipca[["video_files_cur"]]
video_files_cur
mean_ <- ccipca[["mean_"]]

setwd(master_dir)
load(paste("ccipca_videoSTRS2013.RData", sep = ""))
components_ <- ccipca[["components_"]]

prx2014 <- (as.matrix(RepFrames_cur) - matrix(rep(mean_, length(video_files_cur)), ncol = length(mean_), byrow = TRUE)) %*% t(components_)
prx2014 <- as.data.frame(prx2014)
names(prx2014) <- paste("Video", seq(1, ncol(prx2014), 1), sep = "")
# convert to numeric
for(name in names(prx2014)){
	prx2014[, name] <- as.numeric(prx2014[, name])
}
# # scale so that variance  = 1
# for(j in 1:ncol(prx2014)) {
# 	if(is.numeric(prx2014[, j]) == TRUE) {
# 		prx2014[, j] <- prx2014[, j]/sd(prx2014[, j])
# 	}
# }
head(prx2014)[, 1:2]
head(prx)[, 1:2]

setwd(master_dir)

load(file = paste("arnn_videoSTRS2013", paste(Outputs, collapse = ""), ".RData", sep = ""))
arnn$call

load(file = paste("rndf_videoSTRS2013", paste(Outputs, collapse = ""), ".RData", sep = ""))
rndf$confusion

clear_rdnf <- video_files_cur[as.vector(predict(object = rndf, newdata = prx2014)) == "Clear"]
clear_rdnf

require(neuralnet)
arnn_result <- compute(x = arnn, covariate = prx2014[, na.omit(as.vector(str_match(names(prx2014), "Video.+")))])
arnn_result <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = arnn_result$net.result)
require(stringr)
clear_arnn <- video_files_cur[is.na(str_match(as.vector(arnn_result[, 1]), ".+Clear")) == FALSE]
clear_arnn

for(clear in seq(1, length(clear_rdnf), 1)) {
	print(paste(clear, " ", clear_rdnf[clear], sep = ""))
}

# deletes cluster folder, then creates it (aviods overlap)
system(paste("rm -r ", "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear", sep = ""))
system(paste("mkdir ", "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear", sep = ""))

load(paste("ccipca_video2014.RData", sep = ""))
video_files_cur <- ccipca[["video_files_cur"]]

system(paste("mkdir ", dest_dir, sep = ""))
for(clear in clear_rdnf) {
	from_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014"
	dest_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear"
	system(paste("cp ", from_dir, "/", clear, ".jpg", " ", dest_dir, sep = ""))
}

ccipca[["Clear"]] <- Clear
ccipca[["Cloudy"]] <- Cloudy

# save.image(paste("ccipca_video", year, ".RData", sep = ""))

# ----------------------------------------
# run feature detection

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2013/Clear/Features"
frame_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2013"
require(stringr)
video_files = str_match(list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/clust2013/Clear", pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]

# to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Features"
# frame_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014"
# require(stringr)
# video_files = str_match(list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear", pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]

setwd(master_dir)
year <- "2013"
source("Video_features.r")

# ----------------------------------------
# train video data on existing setwd

setwd(master_dir)
N = 10

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))
video_files_cur <- ccipca[["video_files_cur"]]

Clear <- character(0)
require(stringr)
Clear <- na.omit(str_match(list.files(paste(clust_dir, "/Clear", sep = "")), "(.+)\\.jpg")[, 2])
Clear
ImageClarity <- ifelse(video_files_cur %in% Clear, "Clear", "Cloudy")
ImageClarity

Biotic <- character(0)
Biotic <- na.omit(str_match(list.files(paste(clust_dir, "/Clear/Biotic", sep = "")), "(.+)\\.jpg")[, 2])
IfBiotic <- ifelse(video_files_cur %in% Biotic, "1", "0")
IfBiotic

Outputs <- c("IfBiotic")

source("Video_training_data.r")
source("Video_training.r")

# ----------------------------------------
# apply clear classifier to 2014 data

Outputs <- c("IfBiotic")

setwd(master_dir)
load(paste("data_areas2014.RData", sep = ""))

setwd(master_dir)
load(paste("ccipca_video2014.RData", sep = ""))
video_files_cur <- ccipca[["video_files_cur"]]
video_files_cur

Clear <- character(0)
require(stringr)
Clear <- na.omit(str_match(list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear"), "(.+)\\.jpg")[, 2])
Clear
ImageClarity <- ifelse(video_files_cur %in% Clear, "Clear", "Cloudy")
ImageClarity

setwd(master_dir)
load(file = paste("arnn_video", "2013", paste(Outputs, collapse = ""), ".RData", sep = ""))
arnn$call

load(file = paste("rndf_video", "2013", paste(Outputs, collapse = ""), ".RData", sep = ""))
rndf$confusion

biotic_rdnf <- Clear[as.vector(predict(object = rndf, newdata = areas)) == "1"]
biotic_rdnf

require(neuralnet)
arnn_result <- compute(x = arnn, covariate = areas[, na.omit(as.vector(str_match(names(areas), "Areas.+")))])
arnn_result <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = arnn_result$net.result)
arnn_result
require(stringr)
biotic_arnn <- Clear[is.na(str_match(as.vector(arnn_result[, 1]), ".+1")) == FALSE]
biotic_arnn

for(biotic in seq(1, length(biotic_rdnf), 1)) {
	print(paste(biotic, " ", biotic_rdnf[biotic], sep = ""))
}

# deletes cluster folder, then creates it (aviods overlap)
system(paste("rm -r ", "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Biotic", sep = ""))
system(paste("mkdir ", "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Biotic", sep = ""))

system(paste("mkdir ", dest_dir, sep = ""))
for(biotic in biotic_rdnf) {
	from_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014"
	dest_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Biotic"
	system(paste("cp ", from_dir, "/", biotic, ".jpg", " ", dest_dir, sep = ""))
}

# deletes cluster folder, then creates it (aviods overlap)
system(paste("rm -r ", "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Biotic", sep = ""))
system(paste("mkdir ", "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Biotic", sep = ""))

system(paste("mkdir ", dest_dir, sep = ""))
for(biotic in biotic_arnn) {
	from_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014"
	dest_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/clust2014/Clear/Biotic"
	system(paste("cp ", from_dir, "/", biotic, ".jpg", " ", dest_dir, sep = ""))
}


# ----------------------------------------

proje_dim = 444
setwd(master_dir)
# source("Video_projection.r")

# ----------------------------------------
# Output Eigentraps

n_pc = 10
setwd(master_dir)
# source("Video_eigenvectors.r")
