setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

to_dir = paste(clust_dir, "/cur/Clear/Features", sep = "")
frame_dir = frame_dir
require(stringr)
if(length(list.files(paste(clust_dir, "/cur/Clear", sep = ""), pattern = "\\.jpg")) > 0) {
	video_files = str_match(list.files(paste(clust_dir, "/cur/Clear", sep = ""), pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]
} else {
	video_files <- vector()
}

# deletes rep_frames folder, then creates it (aviods overlap)
system(paste('rm -r ', to_dir, sep = ""))
system(paste('mkdir ', to_dir, sep = ""))

video_file = video_files[1]
video_file

Detect_features <- function(video_file, frame_dir, to_dir, W, H, skip) {

	print(paste("Detecting features: ", video_file, sep = ""))

	require(rPython)
	setwd(master_dir)
	python.assign("video_file", video_file)
	python.assign("frame_dir", frame_dir)
	python.assign("to_dir", to_dir)
	python.assign("skip", skip)
	python.assign("W", W)
	python.assign("H", H)
	python.assign("data", 0)
	python.load("Mean_frame.py")
	python.load("Reduce_colors.py")
	python.load("Remove_trap.py")
	python.load("Resize_frame.py")
	python.load("Detect_features.py")
	python.exec("
  os.chdir(frame_dir)
  frame = cv2.imread(video_file + '.jpg')
  frame = Remove_trap(frame)
  data_areas = Detect_features( frame, to_dir, video_file + '.jpg' )
")
  # frame = Resize_frame(frame, W, H)

	data_areas = python.get("data_areas")

	if(is.numeric(data_areas) == TRUE) {
		data_areas <- c(summary(data_areas), length(data_areas[data_areas != 0]), ifelse(is.na(sd(data_areas)), 0, sd(data_areas)))
	} else {
		data_areas <- rep(0, 8)
	}
	names(data_areas) <- paste("Areas", seq(1, length(data_areas), 1), sep = "")
	print(data_areas)

	return(data_areas)

}

if(mac == TRUE) {
	data_areas = lapply(video_files, FUN = function(video_file) Detect_features(video_file
		, frame_dir, to_dir, W, H, skip))
} else {
	data_areas = mclapply(video_files, FUN = function(video_file) Detect_features(video_file
		, frame_dir, to_dir, W, H, skip)
		, mc.cores = n_cores
		, mc.silent = FALSE
		, mc.preschedule = TRUE
		)

}

data_areas <- matrix(unlist(data_areas), nrow = length(data_areas), byrow = TRUE)
data_areas <- as.data.frame(data_areas)
names(data_areas) <- paste("Areas", seq(1, ncol(data_areas), 1), sep = "")
row.names(data_areas) <- video_files
data_areas$Filenames <- video_files

setwd(master_dir)
save(data_areas, file = paste("data_areas", year, ".RData", sep = ""))

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




