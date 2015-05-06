# ----------------------------------------
# Output projections

video_files_cur <- as.vector(read.table(paste("video_files_cur", year, ".txt", sep = ""))[, 1])
iteration <- length(video_files_cur)

proje_dim_f = formatC(proje_dim, flag = "0", width = 4)
system(paste("rm -r ", proje_dir, "/", proje_dim_f, sep = ""))
system(paste("mkdir ", proje_dir, "/", proje_dim_f, sep = ""))
dest_dir = paste(proje_dir, "/", proje_dim_f, sep = "")

prx <- read.table(paste("prx", year, ".txt", sep = ""))

require(data.table)
components_ <- fread(input = paste("components_", year, ".txt", sep = ""))
# components_ <- read.table("components_.txt")
components_ <- t(components_)
components_ <- Standardize_components(components_)
if(nrow(components_) < n_components) {
  n_zeroes <- n_components - nrow(components_)
  zeroes <- matrix(0, nrow = n_zeroes, ncol = 3*H*W)
  components_ <- rbind(components_, zeroes)
}
mean_ <- read.table(paste("mean_", year, ".txt", sep = ""))

prx <- prx[, 1:proje_dim]
components_ <- components_[1:proje_dim, ]

# projection
proj = (prx[, 1:proje_dim] %*% components_[1:proje_dim, ]) + matrix(rep(mean_, length(video_files_cur)), nrow = length(video_files_cur), byrow = TRUE)

for(i in seq(1, nrow(prx), 1)) {
  
	video_file = video_files_cur[i]
	data = proj[i, ]
  name = paste("proj", "I", formatC(iteration, flag = "0", width = 4), "_", formatC(proje_dim_f, flag = "0", width = 4), "_", video_file, sep = "")

	Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

}
