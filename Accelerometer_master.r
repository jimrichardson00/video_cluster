require(parallel)
n_cores = detectCores(all.tests = FALSE, logical = FALSE) - 1

require(doParallel)
registerDoParallel(cores = n_cores)

year = "2013"
master_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster"
# master_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster"
frame_dir = paste(master_dir, "/frame", year, sep = "")
video_dir = paste(master_dir, "/video", year, sep = "")
audio_dir = paste(master_dir, "/audio", year, sep = "")

# --------------------------------

setwd(master_dir)
source("Accelerometer_functions.r")

# --------------------------------

setwd(master_dir)
N <- 100
V = TRUE
Outputs <- c("TrapMotion")
# Outputs <- c("Sounds")
# Outputs <- c("DominantSubstrate")
source("Accelerometer_training.r")

