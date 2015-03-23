# --------------------------------------------
# Output:
# - Loads principal component analysis (ccipca) if it exists, otherwise sets initial values. 
# - Checks frame_dir, and (clust_dir)/ImageClarity_validated.
# - Subsets to .jpg files that are in frame_dir, but not in (clust_dir)/Image_clarity_validated.
# - For these files:
	# Reads .jpg file into Python
  # Removes trap by selecting upper middle square of image. This is set in Video_master.r by the parameters x1, x2, y1, y2.
  # Resizes the image in the upper middle square to W x H. W, H are set in Video_master.r
  # Copies the data from the resulting image into a matrix.
# - If principal component analysis (ccipca) already exists from previous data:
    # Adds new data to current ccipca
# - If no ccipca exists:
    # Runs a ccipca on this matrix.
  # Writes ccipca info (mean, components, iteration, copy, amnesic) to .txt files. 
# - Reads in .txt files to R and saves all info (including data matrix, mean, components, and projection of data to components) to .RData file
# --------------------------------------------

# --------------------------------------------
# - Loads principal component analysis (ccipca) if it exists, otherwise sets initial values. 
setwd(master_dir)
if(file.exists(paste("ccipca_video", year, ".RData", sep = "")) == TRUE & rerun == 0) {

  setwd(master_dir)
  load(paste("ccipca_video", year, ".RData", sep = ""))

  n_components <- as.integer(ccipca[["n_components"]])
  iteration <- as.integer(ccipca[["iteration"]]) 
  amnesic <- as.integer(ccipca[["amnesic"]]) 
  copy <- as.integer(ccipca[["copy"]]) 
  mean_ <- ccipca[["mean_"]] 
  components_ <- ccipca[["components_"]] 
  RepFrames_cur <- ccipca[["RepFrames_cur"]] 
  video_files_cur <- ccipca[["video_files_cur"]]

} else {
  
  RepFrames_cur <- data.frame()
  video_files_cur <- vector()
  SetTraps_cur <- vector()
  video_files_frames_cur <- vector()
  n_components <- 0
  components_ <- data.frame()
  iteration <- 0

}

# --------------------------------------------
# - Checks frame_dir, and (clust_dir)/ImageClarity_validated.
# - Subsets to .jpg files that are in frame_dir, but not in (clust_dir)/Image_clarity_validated.

# list of videos in video folder
require(stringr)
video_files <- str_match(sort(list.files(frame_dir)), "(.+)\\.jpg")[, 2]
video_files

# defines new videos to be added
video_files_new <- video_files[!(video_files %in% video_files_cur)]
video_files_new

# cacluates the new rep frames matrix
if(length(video_files_new) > 0) {

  setwd(frame_dir)
  require(parallel)

  print(paste("Adding: ", length(video_files_new), " new files", sep = ""))

	# Reads .jpg file into Python
  # Removes trap by selecting upper middle square of image. This is set in Video_master.r by the parameters x1, x2, y1, y2.
  # Resizes the image in the upper middle square to W x H. W, H are set in Video_master.r
  # Copies the data from the resulting image into a matrix RepFrames_new
  if(mac == TRUE) {
    RepFrames_new <- lapply(video_files_new, 
      FUN = function(video_file) Extract_frame(video_file = video_file, 
        W = W, H = H, frame_dir = frame_dir, skip = skip)
      )
  } else {
    RepFrames_new <- mclapply(video_files_new, 
      FUN = function(video_file) Extract_frame(video_file = video_file, 
        W = W, H = H, frame_dir = frame_dir, skip = skip)
      , mc.cores = n_cores
      , mc.silent = FALSE
      , mc.preschedule = TRUE
      )
  }
  RepFrames_new <- unlist(RepFrames_new)
  RepFrames_new <- matrix(RepFrames_new, ncol = 3*W*H, byrow = TRUE)

  # setwd(master_dir)
  # save(RepFrames_new, file = paste("RepFrames_new", year, ".RData", sep = ""))
  # print(doesnt_exist)
  # setwd(master_dir)
  # load(paste("RepFrames_new", year, ".RData", sep = ""))
  
  # sets n_components as total number of video_files
  n_components <- length(video_files_new) + length(video_files_cur)
  n_components

	# - If principal component analysis (ccipca) already exists from previous data:
	#     Adds new data to current ccipca
	# - If no ccipca exists:
	#     Runs a ccipca on this matrix.
	#   Writes ccipca info (mean, components, iteration, copy, amnesic) to .txt files. 
  print(paste("Starting: CCIPCA_RepFrames;", 
    " new ", length(video_files_new), 
    " cur ", length(video_files_cur), sep = ""))
  if(mac == TRUE) {
    ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = n_components, rerun = rerun, year = year)
  } else {
    # ccipca_dict <- CCIPCA_RepFrames(RepFrames_new = RepFrames_new, n_components = n_components, rerun = rerun, year = year)
  }
  print(paste("Finished: CCIPCA_RepFrames;", 
    " new ", length(video_files_new), 
    " cur ", length(video_files_cur), sep = ""))


	# - Reads in .txt files to R and saves all info (including data matrix, mean, components, and projection of data to components) to .RData file

  # updated values
  print("n_components")
  n_components <- read.table("n_components.txt")  
  print("iteration")
  iteration <- read.table("iteration.txt")  
  print("amnesic")
  amnesic <- read.table("amnesic.txt") 
  print("copy")
  copy <- read.table("copy.txt")   
  require(data.table)
  print("mean_")
  require(data.table)
  mean_ <- fread(input = "mean_.txt")
  mean_ <- as.vector(mean_$V1)
  print("components_")
  components_ <- fread(input = "components_.txt")
  # components_ <- read.table("components_.txt")
  components_ <- t(components_)
  components_ <- Standardize_components(components_)
  if(nrow(components_) < n_components) {
    n_zeroes <- n_components - nrow(components_)
    zeroes <- matrix(0, nrow = n_zeroes, ncol = 3*H*W)
    components_ <- rbind(components_, zeroes)
  }
  RepFrames_cur <- rbind(RepFrames_cur, RepFrames_new)
  video_files_cur <- c(video_files_cur, video_files_new)
  prx <- (as.matrix(RepFrames_cur) - matrix(rep(mean_, length(video_files_cur)), ncol = length(mean_), byrow = TRUE)) %*% t(components_)

  # update ccipca list with new values
  ccipca <- list()
  ccipca[["n_components"]] <- as.integer(n_components)  
  ccipca[["iteration"]] <- as.integer(iteration) 
  ccipca[["amnesic"]] <- as.integer(amnesic)
  ccipca[["copy"]] <- as.integer(copy)  
  ccipca[["mean_"]] <- mean_  
  ccipca[["components_"]] <- components_  
  ccipca[["RepFrames_cur"]] <- RepFrames_cur  
  ccipca[["video_files_cur"]] <- video_files_cur  
  ccipca[["prx"]] <- prx

  print(paste("New mean: ", mean(mean_), sep = ))
  print(paste("New components_: ", paste(components_[1, 1:2], collapse = ", "), sep = ))

  # write ccip_videoca list to RData
  setwd(master_dir)
  save(ccipca, file = paste('ccipca_video', year, '.RData', sep = ''))
  print(paste("Saved: ", paste('ccipca_video', year, '.RData', sep = ''), sep = ""))

}
