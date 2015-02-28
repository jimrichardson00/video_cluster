require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1

require(doParallel)
registerDoParallel(cores = n_cores)

W = 192
H = 108
skip = 6
year = "2013"
# year = "2014"
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

# --------------------------------------------
# sources functions needed
setwd(master_dir)
source("Video_functions.r")

# --------------------------------------------
# copies videos into one folder with labels

from_dirs = c("/home/jim/Desktop/october 2013 video")
from_idxs = c("STRS2013")
to_dir =  video_dir
# source("Video_prepfile.r")

# --------------------------------------------
# applies ccipca on video data and saves it to rdata

rerun = 1
fast = 0
setwd(master_dir)
# source("Video_ccipca.r")

# ---------------------------------------------------
# copies frames into cluster folders, and into rep_frames folder

k = 40
setwd(master_dir)
# source("Video_cluster.r")

# ----------------------------------------
# train video data on existing setwd

setwd(master_dir)
N = 10
# Outputs <- c("DominantSubstrate")
# Outputs <- c("SpeciesCount")
Outputs <- c("ImageClarity")
source("Video_training.r")

# ----------------------------------------

proje_dim = 444
setwd(master_dir)
# source("Video_projection.r")

# ----------------------------------------
# Output Eigentraps

n_pc = 10
setwd(master_dir)
# source("Video_eigenvectors.r")

# ----------------------------------------

load(paste("ccipca_video2014.RData", sep = ""))
RepFrames_cur <- ccipca[["RepFrames_cur"]]
video_files_cur <- ccipca[["video_files_cur"]]
mean_ <- ccipca[["mean_"]]

load(paste("ccipca_video2013.RData", sep = ""))
components_ <- ccipca[["components_"]]

prx2014 <- (as.matrix(RepFrames_cur) - matrix(rep(mean_, length(video_files_cur)), ncol = length(mean_), byrow = TRUE)) %*% t(components_)
prx2014 <- as.data.frame(prx2014)
names(prx2014) <- paste("Video", seq(1, ncol(prx2014), 1), sep = "")
# convert to numeric
for(name in names(prx2014)){
	prx2014[, name] <- as.numeric(prx2014[, name])
}
# scale so that variance  = 1
for(j in 1:ncol(prx2014)) {
	if(is.numeric(prx2014[, j]) == TRUE) {
		prx2014[, j] <- prx2014[, j]/sd(prx2014[, j])
	}
}

setwd(master_dir)

load(file = paste("arnn_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))
arnn$call

load(file = paste("rndf_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))
rndf$confusion

clear_rdnf <- video_files_cur[as.vector(predict(object = rndf, newdata = prx2014)) == "Clear"]
clear_rdnf

require(neuralnet)
arnn_result <- compute(x = arnn, covariate = prx2014[, paste("Video", seq(1, 444, 1), sep = "")])
arnn_result <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = arnn_result$net.result)
require(stringr)
clear_arnn <- video_files_cur[is.na(str_match(as.vector(arnn_result[, 1]), ".+Clear")) == FALSE]
clear_arnn

for(clear in seq(1, length(clear_arnn), 1)) {
	print(paste(clear, " ", clear_arnn[clear], sep = ""))
}

# deletes cluster folder, then creates it (aviods overlap)
# system(paste("rm -r ", frame_dir, "/", "Clear", sep = ""))
system(paste("mkdir ", "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014/Clear", sep = ""))

load(paste("ccipca_video2014.RData", sep = ""))
video_files_cur <- ccipca[["video_files_cur"]]

for(clear in clear_arnn){

	name = clear
	data = ccipca[["RepFrames_cur"]][video_files_cur == name, ]

	dest_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014/Clear"
	Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)
}

# ----------------------------------------
# run feature detection

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014/Clear/Features"
video_files = str_match(list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014/Clear", pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]
video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video2014"

setwd(master_dir)
source("Video_features.r")


