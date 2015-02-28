require(rPython)

year = "2013"
master_dir = "/Users/jimrichardson/Dropbox/REM/Tasks/video_cluster"
video_dir = paste(master_dir, "/video", year, sep = "")
n_cores = 2

setwd(master_dir)

Hello_world <- function(video_file) {
	python.load("Test.py")
	output <- python.call("Hello_world", video_file, master_dir)
	return(output)
}

print("Starting single file")

video_file <- "STRS2013_S0004T0014_GOPR0606.mp4"
Hello_world(video_file)

print("Starting multiple file")

video_files <- c("STRS2013_S0004T0014_GOPR0606.mp4", "STRS2013_S0004T0014_GOPR0607.mp4")

require(parallel)
# output <- mclapply(video_files, FUN = function(video_file) Hello_world(video_file)
# 	, mc.cores = 2
# 	, mc.preschedule = FALSE)

require(doParallel)
registerDoParallel(cores = n_cores)
registerDoParallel(cores = 2)
output <- foreach(video_file = video_files) %dopar% {
	Hello_world(video_file)
}

print(output)

