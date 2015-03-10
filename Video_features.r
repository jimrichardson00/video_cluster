setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

# deletes rep_frames folder, then creates it (aviods overlap)
system(paste('rm -r ', to_dir, sep = ""))
system(paste('mkdir ', to_dir, sep = ""))

video_file = video_files[1]
video_file

Detect_features <- function(video_file, frame_dir, to_dir, W, H, skip) {

	print(video_file)

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
  areas = Detect_features( frame, to_dir, video_file + '.jpg' )
")
  # frame = Resize_frame(frame, W, H)

	areas = python.get("areas")

	if(is.numeric(areas) == TRUE) {
		areas <- c(summary(areas), length(areas[areas != 0]), ifelse(is.na(sd(areas)), 0, sd(areas)))
	} else {
		areas <- rep(0, 8)
	}
	names(areas) <- paste("Areas", seq(1, length(areas), 1), sep = "")
	print(areas)

	return(areas)

}

video_file <- "STRS2013_S022T014_GOPR0342"
video_file <- video_files[1]
Detect_features(video_file, frame_dir, to_dir, W, H, skip)
video_files

areas = mclapply(video_files, FUN = function(video_file) Detect_features(video_file, frame_dir, to_dir, W, H, skip)
	, mc.cores = n_cores
	, mc.silent = FALSE
	, mc.preschedule = TRUE
	)

areas <- matrix(unlist(areas), nrow = length(areas), byrow = TRUE)
areas <- as.data.frame(areas)
names(areas) <- paste("Areas", seq(1, ncol(areas), 1), sep = "")
row.names(areas) <- video_files
areas$Filenames <- video_files

setwd(master_dir)
save(areas, file = paste("data_areas", year, ".RData", sep = ""))

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








