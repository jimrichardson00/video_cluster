# ----------------------------------------------------------------
# extracts data from each video and pulls out representative frame

# checks if there is a current ccipca file
# and loads variables if there is:
setwd(master_dir)
if(file.exists(paste("ccipca_video", year, ".RData", sep = "")) == TRUE & rerun == 0) {

	setwd(master_dir)
	load(paste("ccipca_video", year, ".RData", sep = ""))

	n_components = ccipca[["n_components"]]
	iteration = ccipca[["iteration"]] 
	amnesic = ccipca[["amnesic"]] 
	copy = ccipca[["copy"]] 
	mean_ = ccipca[["mean_"]] 
	components_ = ccipca[["components_"]] 
	RepFrames_cur = ccipca[["RepFrames_cur"]] 
	video_files_cur = ccipca[["video_files_cur"]]
	# SetTraps_cur = ccipca[["SetTraps"]]

} else {
	
	RepFrames_cur <- data.frame()
	video_files_cur <- vector()
	SetTraps_cur <- vector()
	video_files_frames_cur <- vector()
	n_components <- 0
	components_ <- data.frame()
	iteration <- 0

}

# list of videos and SetTraps in video folder
require(stringr)
video_files <- str_match(sort(list.files(frame_dir)), "(.+)\\.jpg")[, 2]
video_files

# defines new videos to be added
video_files_new <- video_files[!(video_files %in% video_files_cur)]
video_files_new

video_file = video_files_new[50]
video_file

# cacluates the new rep frames matrix
if(length(video_files_new) > 0) {

	setwd(frame_dir)
	require(parallel)

	print(paste("Adding: ", length(video_files_new), " new files", sep = ""))

	if(mac == TRUE) {
		# video_files_frames_new_Ll <- lapply(video_files_new, 
		# 	FUN = function(video_file) Extract_RepFrame(video_file = video_file, 
		# 		W = W, H = H, video_dir = video_dir, skip = skip, fast = fast))
		# RepFrames_new_Ll <- lapply(unlist(video_files_frames_new_Ll), 
		# 	FUN = function(video_file_frame) Extract_data(video_file_frame, 
		# 		W = W, H = H, video_dir = video_dir))
		# RepFrames_new_Ll <- lapply(video_files, 
		# 	FUN = function(video_file) Extract_MeanFrame(video_file = video_file, 
		# 		W = W, H = H, video_dir = video_dir, skip = skip))		
		RepFrames_new <- lapply(video_files, 
			FUN = function(video_file) Extract_frame(video_file = video_file, 
				W = W, H = H, frame_dir = frame_dir, skip = skip)
			)
		# Len <- unlist(lapply(seq(1, length(RepFrames_new_Ll), 1), FUN = function(i) length(RepFrames_new_Ll[[i]])))
	} else {
		# video_files_frames_new_Ll <- mclapply(video_files_new, 
		# # video_files_frames_new_Ll <- lapply(video_files_new, 
		# 	FUN = function(video_file) Extract_RepFrame(video_file = video_file, 
		# 		W = W, H = H, video_dir = video_dir, skip = skip, fast = fast)
		# 	, mc.cores = n_cores
		# 	, mc.silent = FALSE
		# 	, mc.preschedule = TRUE
		# 	)
		# RepFrames_new_Ll <- mclapply(unlist(video_files_frames_new_Ll), 
		# # RepFrames_new_Ll <- lapply(unlist(video_files_frames_new_Ll), 
		# 	FUN = function(video_file_frame) Extract_data(video_file_frame, 
		# 		W = W, H = H, video_dir = video_dir)
		# 	, mc.cores = n_cores
		# 	, mc.silent = FALSE
		# 	, mc.preschedule = TRUE
		# 	)
		# RepFrames_new_Ll <- lapply(video_files, 
		# RepFrames_new_Ll <- mclapply(video_files, 
		# 	FUN = function(video_file) Extract_MeanFrame(video_file, 
		# 		W = W, H = H, video_dir = video_dir, skip = skip)
		# 	, mc.cores = n_cores
		# 	, mc.silent = FALSE
		# 	, mc.preschedule = TRUE
		# 	)
		RepFrames_new <- mclapply(video_files, 
			FUN = function(video_file) Extract_frame(video_file = video_file, 
				W = W, H = H, frame_dir = frame_dir, skip = skip)
			, mc.cores = n_cores
			, mc.silent = FALSE
			, mc.preschedule = TRUE
			)
		# Len <- unlist(mclapply(seq(1, length(RepFrames_new_Ll), 1), FUN = function(i) length(RepFrames_new_Ll[[i]])
		# # Len <- unlist(lapply(seq(1, length(RepFrames_new_Ll), 1), FUN = function(i) length(RepFrames_new_Ll[[i]])
		# 	, mc.cores = n_cores
		# 	, mc.silent = FALSE
		# 	, mc.preschedule = TRUE
		# 	))
	}
	RepFrames_new <- unlist(RepFrames_new)
	RepFrames_new <- matrix(RepFrames_new, ncol = 3*W*H, byrow = TRUE)

	# setwd(master_dir)
	# save(RepFrames_new, file = paste("RepFrames_new", year, ".RData", sep = ""))

	# # # ----------------------------------

	# setwd(master_dir)
	# load(paste("RepFrames_new", year, ".RData", sep = ""))
	
	# add new points to current pca
	print(paste("Starting: CCIPCA_RepFrames;", 
		" new ", length(video_files_new), 
		" cur ", length(video_files_cur), sep = ""))
	if(mac == TRUE) {
		ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = nrow(RepFrames_new), rerun = rerun)
	} else {
		ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = 10, rerun = rerun)
	}
	print(paste("Finished: CCIPCA_RepFrames;", 
		" new ", length(video_files_new), 
		" cur ", length(video_files_cur), sep = ""))

	# updated values
	n_components <- ccipca_dict[["n_components"]]  
	iteration <- ccipca_dict[["iteration"]]  
	amnesic <- ccipca_dict[["amnesic"]] 
	copy <- ccipca_dict[["copy"]]   
	mean_ <- ccipca_dict[["mean_"]]
	components_ <- matrix(unlist(ccipca_dict[["components_"]]), ncol = 3*W*H, byrow = TRUE)
	components_ <- Standardize_components(components_)
	if(nrow(components_) < n_components) {
		n_zeroes <- n_components - nrow(components_)
		zeroes <- matrix(0, nrow = n_zeroes, ncol = 3*H*W)
		components_ <- rbind(components_, zeroes)
	}
	RepFrames_cur <- rbind(RepFrames_cur, RepFrames_new)
	video_files_cur <- c(video_files_cur, video_files_new)
	# video_files_frames_cur <- c(video_files_frames_cur, video_files_frames_new)
	prx <- (as.matrix(RepFrames_cur) - matrix(rep(mean_, length(video_files_cur)), ncol = length(mean_), byrow = TRUE)) %*% t(components_)

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
	# ccipca[["video_files_frames_cur"]] <- video_files_frames_cur  
	ccipca[["prx"]] <- prx

	print(paste("New mean: ", mean(mean_), sep = ))

	# write ccip_videoca list to RData
	setwd(master_dir)
	save(ccipca, file = paste('ccipca_video', year, '.RData', sep = ''))

}
