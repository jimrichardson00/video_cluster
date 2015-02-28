# ----------------------------------------
# Output projections

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

proje_dim_f = formatC(proje_dim, flag = "0", width = 4)
system(paste("rm -r ", proje_dir, "/", proje_dim_f, sep = ""))
system(paste("mkdir ", proje_dir, "/", proje_dim_f, sep = ""))
dest_dir = paste(proje_dir, "/", proje_dim_f, sep = "")

prx <- ccipca[["prx"]]
components_ <- ccipca[["components_"]]
mean_ <- ccipca[["mean_"]]
video_files_frames_cur <- ccipca[["video_files_frames_cur"]]

prx <- prx[, 1:proje_dim]
components_ <- components_[1:proje_dim, ]

# projection
proj = (prx[, 1:proje_dim] %*% components_[1:proje_dim, ]) + matrix(rep(mean_, length(video_files_frames_cur)), nrow = length(video_files_frames_cur), byrow = TRUE)

for(i in seq(1, nrow(prx), 1)) {
  
	video_files_frame = video_files_frames_cur[i]

	data = proj[i, ]
  		
	name = paste("proj", formatC(proje_dim_f, flag = "0", width = 4), "_", video_files_frame, sep = "")

	Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

}

RepFrames_cur <- ccipca[["RepFrames_cur"]]

head(proj[1, ])
head(RepFrames_cur[1, 1:10])

mean_[1:10]

(prx[, 1:proje_dim] %*% components_[1:proje_dim, ])[1, 1:10]

