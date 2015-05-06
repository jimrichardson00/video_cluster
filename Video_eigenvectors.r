# ----------------------------------------
# Output Eigentraps

setwd(master_dir)

video_files_cur <- as.vector(read.table(paste("video_files_cur", year, ".txt", sep = ""))[, 1])
iteration <- length(video_files_cur)

for(pc in seq(1, n_pc, 1)){

  name <- paste("prcomp", "I", formatC(iteration, flag = "0", width = 4), "_", formatC(pc, flag = "0", width = 4), sep = "")

	data <- read.table(paste("components_", year, ".txt", sep = ""))[pc, ]
	data <- (255/max(data))*data
	data[data < 0] <- 0

	Output_image(name = name, data = data, dest_dir = eigen_dir, W = W, H = H)

}
