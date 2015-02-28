require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1
n_cores

require(doParallel)
registerDoParallel(cores = n_cores)

year = "2013"
# mac = FALSE
mac = TRUE
if(mac == TRUE) {
	master_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster"
	n_cores = 11
} else {
	master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster"
}
frame_dir = paste(master_dir, "/frame", year, sep = "")
video_dir = paste(master_dir, "/video", year, sep = "")
audio_dir = paste(master_dir, "/audio", year, sep = "")

# --------------------------------------------
# sources functions needed
setwd(master_dir)
source("Audio_functions.r")

# ------------------------------------------------------
# extracts audio from video files and copies it to audio folder

setwd(master_dir)
# source("Audio_prepfile.r")

# --------------------------------------------
# applies pca on audio data and saves it to rdata

setwd(master_dir)
# source("Audio_pca.r")

# ---------------------------------------------------
# copies frames into cluster folders

setwd(master_dir)
# source("Audio_cluster.r")

# ----------------------------------------
# train audio data on existing data

setwd(master_dir)
N = 100
# Outputs = c("DominantSubstrate")
Outputs = c("Sounds")
if("DominantSubstrate" %in% Outputs) {
	# Sounds = FALSE
	Sounds = TRUE
} else {
	Sounds = FALSE
}
# source("Audio_training.r")


