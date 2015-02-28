setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

# to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014/Clear/Features"
# video_files = str_match(list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/frame2014/Clear", pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]
# video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video2014"

to_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/frame2013/Clear/Features"
video_files = str_match(list.files("/home/jim/Dropbox/REM/Tasks/video_cluster/frame2013/Clear", pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]
video_dir = "/home/jim/Dropbox/REM/Tasks/video_cluster/video2013"

# to_dir <- paste(frame_dir, "/", "Clear", "/", "Features", sep = "")
# to_dir

# deletes rep_frames folder, then creates it (aviods overlap)
system(paste('rm -r ', to_dir, sep = ""))
system(paste('mkdir ', to_dir, sep = ""))

for(video_file in video_files) {

	print(video_file)

	require(rPython)
	setwd(master_dir)
	python.load("Features.py")
	python.call("Features", video_file, W, H, video_dir, skip, to_dir )

}

# ----------------------------------------------------

# video_files <- video_files_cur[ImageClarity == "Clear"]
# video_files

# to_dir <- master_dir
# to_dir <- paste(frame_dir, "/", "Clear", "/", "features", sep = "")

# video_file <- "STRS2013_S0036T0014_GOPR0347.mp4"
# # video_file <- "STRS2013_S0068T0014_GOPR0302.mp4"

# require(rPython)
# setwd(master_dir)
# python.load("Features.py")
# python.call("Features", video_file, W, H, video_dir, skip, to_dir )









