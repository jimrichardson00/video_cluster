# ----------------------------------------
# Output Eigentraps

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

for(pc in seq(1, n_pc, 1)){

	name <- paste("prcompTWO", formatC(pc, flag = "0", width = 2), sep = "")

	data <- ccipca[["components_"]][pc, ]
	data <- (255/max(data))*data

	dest_dir = master_dir

	Output_image(name = name, data, dest_dir, W, H)

}
