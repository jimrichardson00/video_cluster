# contains the functions needed for video_master

posVar <- function(data) {
	out <- lapply(data, function(x) length(unique(x)))
	want <- which(out > 1)
	unlist(want)
}

Diff_from_mode <- function(prx) {
	for(j in seq(1, ncol(prx), 1)) {
		modej = density(prx[, j])$x[which.max(density(prx[, j])$y)]
		prx[, j] <- prx[, j] - modej
	}
	return(mean(apply(prx, MARGIN = 2, FUN = function(x) mean(sum(x^2)))))
}

Extract_data <- function(video_file_frame, W, H, video_dir) {
	require(rPython)
	print(paste("Starting: ", video_file_frame, sep = ""))
	setwd(master_dir)
	python.load("Extract_data.py")
	frame = python.call("Extract_data", video_file_frame, W, H, video_dir)
	print(head(frame[1:10]))
	print(paste("Finished: ", video_file_frame, sep = ""))
	return(frame)
}

Extract_RepFrame <- function(video_file, W, H, skip, fast, video_dir) {

	print(paste("Starting RepFrame: ", video_file, sep = ""))

	require(rPython)

	setwd(master_dir)

	frame_rate = 30

	python.load("Extract_RepFrame.py")
	output <- try(python.call("Extract_RepFrame", video_file, W, H, skip, fast, video_dir))

	if("try-error" %in% class(output) | fast == 1) {

		rep_idx <- 10
	
	} else {

		output <- python.call("Extract_RepFrame", video_file, W, H, skip, fast, video_dir)

		frame_rate <- output[[1]]

		prx <- matrix(unlist(output[[2]]), nrow = length(output[[2]]), byrow = TRUE)

		require(MASS)
		dens <- kde2d(x = prx[, 1], y = prx[, 2])

		pc1 = dens$x[which(dens$z == max(dens$z), arr.ind = TRUE)[1]]
		pc2 = dens$y[which(dens$z == max(dens$z), arr.ind = TRUE)[2]]

		mm <- prx[, 1:2] - matrix(c(rep(pc1, nrow(prx)), rep(pc2, nrow(prx))), ncol = 2, byrow = FALSE)
		mm <- as.data.frame(mm)
		mm[, 3] <- mm[, 1]^2 + mm[, 2]^2

		rep_idx <- which.min(mm[, 3]) + skip - 1

	}

	require(stringr)
	video_file_frame <- paste(str_match(video_file, "(.+)\\.mp4")[, 2], "_F", formatC(rep_idx*frame_rate, flag = "0", width = 4), sep = "")

	print(paste("Finished RepFrame: ", video_file_frame, sep = ""))

	return(video_file_frame)

}

Video_var <- function(video_file, W, H, skip = 6) {

	print(video_file)

	setwd(master_dir)
	python.load("Video_var.py")
	Video_var <- python.call("Video_var", video_file, W, H, skip)

	return(Video_var)
}

Video_mod <- function(video_file, W, H, skip = 6) {

	fast = 0

	setwd(master_dir)
	python.load("Extract_RepFrame.py")
	output <- python.call("Extract_RepFrame", video_file, W, H, skip, fast)

	framesData <- matrix(unlist(output[[1]]), ncol = 3*W*H, byrow = TRUE)
	components_ <- matrix(unlist(output[[2]]), ncol = 3*W*H, byrow = TRUE)

	prx <- t(components_ %*% t(framesData))

	for(j in seq(1, ncol(prx), 1)) {
		mode = density(prx[, j])$x[which.max(density(prx[, j])$y)]
		prx[, j] <- prx[, j] - mode
	}

	return(mean(apply(prx, MARGIN = 2, FUN = function(x) mean(sum(x^2)))))
}

CCIPCA_RepFrames <- function(RepFrames_new, n_components, rerun) {

	require(rPython)

	# turn RepFrames into matrix for python code
	RepFrames_new <- matrix(unlist(RepFrames_new[1:nrow(RepFrames_new), 1:ncol(RepFrames_new)]), nrow = nrow(RepFrames_new), byrow = FALSE)

	# n_components <- n_components + nrow(RepFrames_new)
	# n_components

	setwd(master_dir)
	if(file.exists(paste("ccipca_video", year, ".RData", sep = "")) == TRUE & rerun == 0) {

		if(nrow(components_) > 0 & nrow(components_) < n_components) {
			n_zeroes <- n_components - nrow(components_)
			zeroes <- matrix(0, nrow = n_zeroes, ncol = 3*H*W)
			components_ <- rbind(components_, zeroes)
		}

		python.assign("iteration", iteration) 
		python.assign("amnesic", amnesic)
		python.assign("copy", copy)  
		python.assign("mean_", mean_)  
		python.assign("components_", components_)  

	}

	python.load("CCIPCA_RepFrames.py")
	ccipca <- python.call("CCIPCA_RepFrames", RepFrames_new, n_components, rerun)

	return(ccipca)

}

CCIPCA_SetTrap <- function(SetTrap, W, H, skip, video_dir) {

	require(rPython)

	print(paste("Starting: ", SetTrap, sep = ""))

	setwd(master_dir)

	python.load("CCIPCA_SetTrap.py")
	output <- python.call("CCIPCA_SetTrap", SetTrap, W, H, skip, video_dir)

	count <- output[[3]]
	prx <- matrix(unlist(output[[1]]), ncol = count, byrow = TRUE)
	video_files_frames <- output[[2]]

	min = min(as.numeric(str_match(video_files_frames, "(.+)_GOPR0(.+)_F(.+)")[, 3]))

	cols <- as.numeric(str_match(video_files_frames, "(.+)_GOPR0(.+)_F(.+)")[, 3]) - min + 1

	video_files <- na.omit(str_match(video_files_frames, paste("(", SetTrap, ".+)", "_F.+", sep = ""))[, 2])

	b <- length(unique(video_files))
	b

	video_files_mod <- unlist(lapply(sort(unique(video_files)), FUN = function(video_file) Diff_from_mode(prx[video_files == video_file, ])))

	bandwidth <- function(x) {
		r <- quantile(x, c(0.25, 0.75))
	    h <- (r[2] - r[1])/1.34
	    0.5 * 4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
	}

	dens <- kde2d(x = prx[, 1], y = prx[, 2], h = c(bandwidth(prx[, 1]), bandwidth(prx[, 2])))

	pc1 = dens$x[which(dens$z == max(dens$z), arr.ind = TRUE)[1]]
	pc2 = dens$y[which(dens$z == max(dens$z), arr.ind = TRUE)[2]]

	# setwd(master_dir)
	# png(paste(SetTrap, ".png", sep = ""))
	# contour(dens)
	# points(prx[, 1:2], col = cols, main = SetTrap)
	# points(pc1, pc2, pch = 16, col = "red")
	# legend("topright", fill = sort(unique(cols)),
	#  legend = paste(unique(sort(cols)) + min - 1, " ", 
	# 		"Mod-", formatC(video_files_mod, flag = "0", width = 9, format = "d"), sep = ""))
	# dev.off()

	mm <- prx[, 1:2] - matrix(c(rep(pc1, nrow(prx)), rep(pc2, nrow(prx))), ncol = 2, byrow = FALSE)
	mm <- as.data.frame(mm)
	mm[, 3] <- mm[, 1]^2 + mm[, 2]^2

	print(paste("OutputIs: ", video_files_frames[which.min(mm[, 3])], sep = ""))

	print(paste("Finished: ", SetTrap, sep = ""))

	return(video_files_frames[which.min(mm[, 3])])
}

Output_image <- function(name = "Output_image", data, dest_dir, W, H) {

	require(rPython)

	setwd(master_dir)

	data <- matrix(data, nrow = 3*W*H)
	data <- as.vector(data[, 1])
	data <- unlist(data)

	data <- data - round(mean(data)) + 127
	data[data > 255] <- 255
	data[data < 0] <- 0

	N <- length(data)/3

	head(data)

	blu = matrix(data[seq(1, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	gre = matrix(data[seq(2, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	red = matrix(data[seq(3, 3*N, 3)], ncol = W, nrow = H, byrow = TRUE)
	
	python.load("Output_image.py")
	python.call("Output_image", name, red, gre, blu, dest_dir, W, H)
}

Image_distance <- function(video_file_frame1, video_file_frame2) {
	setwd(master_dir)
	require(rPython)
	python.load("Image_distance.py")
	dist <- python.call("Image_distance", video_files_frames_cur[1], video_files_frames_cur[2], video_dir, H, W)
	print(paste("Dist = ", dist, sep = ""))
	return(dist)
}


collapse <- function(Outputs, Outputs_f, result) {

	result_collapse <- matrix(NA, ncol = length(Outputs), nrow = nrow(result))

	o <- 1
	for(o in seq(1, length(Outputs), 1)) {

		Output <- Outputs[o]

		Output_f_o <- na.omit(str_match(Outputs_f, paste(Output, ".+", sep = "")))

		result_o <- result[, Outputs_f %in% Output_f_o]
		result_o <- as.data.frame(result_o)

		result_o_max.idx <- which(result_o == apply(result_o, MARGIN = 1, FUN = max), arr.in=TRUE)
		result_o_max.idx <- result_o_max.idx[order(result_o_max.idx[, 1]), ]

		result_o_max <- Output_f_o[result_o_max.idx[, 2]]
		result_o_max <- as.vector(result_o_max)

		result_collapse[, o] <- result_o_max

	}

	result_collapse <- as.data.frame(result_collapse)
	names(result_collapse) <- Outputs
	return(result_collapse)
}

confusion <- function(i, Output, results, var1 = "data_result", var2) {

	Outputs_f_o <- na.omit(str_match(Outputs_f, paste(Output, ".+", sep = "")))
	Outputs_f_o

	Var1 <- results[[i]][[var1]][, Output]
	Var2 <- results[[i]][[var2]][, Output]

	tab <- table(results[[i]][[var1]][, Output], results[[i]][[var2]][, Output])
	tab <- as.data.frame(tab)
	tab <- rbind(tab, expand.grid(Outputs_f_o, Outputs_f_o, Freq = 0))
	tab <- xtabs(Freq ~ Var1 + Var2, data = tab)

	df <- matrix(NA, ncol = ncol(tab), nrow = nrow(tab))
	df <- tab[,]
	df <- as.data.frame(df)

	df <- df[order(row.names(df)),]
	if(length(df) > 1){
		df <- df[, order(names(df))]
	}

	return(df)
}

per_correct <- function(confusion) {
	confusion <- as.matrix(confusion)
	return( sum(diag(confusion)) / sum(confusion) )
}

Extract_MeanFrame <- function(video_file, W, H, video_dir, skip) {

	print(paste("Starting Mean frame: ", video_file, sep = ""))

	require(rPython)

	setwd(master_dir)
	python.load("Mean_frame.py")
	frame = python.call("Mean_frame", video_file, W, H, video_dir, skip)

	print(paste("Finished Mean frame: ", video_file, sep = ""))

	return(frame)
}

Standardize_components <- function(components_) {
	for(j in seq(1, nrow(components_))) {
		if(components_[j, 1] < 0) {
			components_[j, ] <- -1*components_[j, ]
		}
	}
	return(components_)
}

Extract_frame <- function(video_file, frame_dir, W, H, skip) {

	print(paste("Starting frame: ", video_file, sep = ""))

	require(rPython)

	setwd(master_dir)
	python.load("Extract_frame.py")
	frame = python.call("Extract_frame", paste(video_file, ".jpg", sep = ""), frame_dir, W, H, skip)
	print(paste("Finished frame: ", video_file, sep = ""))

	return(frame)
}

