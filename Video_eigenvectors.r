# ----------------------------------------
# Output Eigentraps

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

video_files_cur <- ccipca[["video_files_cur"]]
iteration <- length(video_files_cur)

for(pc in seq(1, n_pc, 1)){

  name <- paste("prcomp", "I", formatC(iteration, flag = "0", width = 4), "_", formatC(pc, flag = "0", width = 4), sep = "")

	data <- ccipca[["components_"]][pc, ]
	data <- (255/max(data))*data
	data[data < 0] <- 0

	Output_image(name = name, data = data, dest_dir = eigen_dir, W = W, H = H)

}
