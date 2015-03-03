# ----------------------------------------------------
# applies hclust clustering

setwd(master_dir)
load(paste("ccipca_video", year, ".RData", sep = ""))

prx <- ccipca[["prx"]]
prx <- as.data.frame(prx)
row.names(prx) <- ccipca[["video_files_cur"]]

# define distance and number of clusters
dist <- dist(prx, method = 'euclidean')

# apply clustering algorithm
fit <- hclust(dist, method = "ward.D2") 

# pull out cluster names
require(dendroextras)
sl <- slice(fit, k = k)
sl <- as.matrix(sl)
sl <- as.data.frame(sl)
sl$rn <- row.names(sl)
sl <- sl[order(sl$rn),]

# define cluster names
clusters <- sl$V1
clusters

# create png of dendogram
setwd(master_dir)

colours <- rainbow(k)
colours

pdf("Video__hclust_dendrogram.pdf")
fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE, col = colours)
plot(fit_col)
require(dendextend)
rect.dendrogram(fit_col, k = k, border = "red")
dev.off()

memb <- cutree(fit, k = k)
cent <- NULL
for(cl in 1:k){
  cent <- rbind(cent, colMeans(prx[memb == cl, , drop = FALSE]))
}

pdf("Video_cluster_hclust.pdf")
plot(prx[, 1:2], col = colours)
points(cent[, 1:2], pch = 16)
text(cent[, 1:2], labels = unique(clusters), pos = 3)
dev.off()

fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE, col = colours)
plot(fit_col)
require(dendextend)
# rect.dendrogram(fit_col, k = k, border = "red")
dev.new()

plot(prx[, 1:2], col = colours)
points(cent[, 1:2], pch = 16)
text(cent[, 1:2], labels = unique(clusters), pos = 3)

# --------------------------------------------------
# applies kmeans clustering

# setwd(master_dir)
# load(paste("ccipca_video", year, ".RData", sep = ""))

# prx <- ccipca[["prx"]]
# prx <- as.data.frame(prx)
# row.names(prx) <- ccipca[["video_files_cur"]]

# # n_compenents d
# k <- 30
# km <- kmeans(ccipca[["prx"]], centers = k) 
# clusters <- km$cluster

# setwd(master_dir)
# png("Video_cluster.png")
# plot(ccipca[["prx"]][, 1], ccipca[["prx"]][, 2], col = km$cluster)
# points(km$centers[, c(1, 2)], pch = 16)
# text(km$centers[, 1:2], labels = sort(unique(clusters)), pos = 3)
# dev.off()

# ---------------------------------------------------

ccipca[["clusters"]] <- clusters
save(ccipca, file = paste('ccipca_video', year, '.RData', sep = ''))

# ---------------------------------------------------
# copies frames into cluster folders, and into rep_frames folder

# # deletes rep_frames folder, then creates it (aviods overlap)
system(paste('rm -r ', clust_dir, sep = ""))
system(paste('mkdir ', clust_dir, sep = ""))

# cycles through clusters
for(cluster in sort(unique(clusters))) {

	print(cluster)

	# deletes cluster folder, then creates it (aviods overlap)
	system(paste("rm -r ", clust_dir, "/", cluster, sep = ""))
	system(paste("mkdir ", clust_dir, "/", cluster, sep = ""))

	video_files_cl <- ccipca[["video_files_cur"]][clusters == cluster]
	video_files_cl

	for(i in seq(1, sum(clusters == cluster), 1)){

		# name = video_files_cl[i]
		# data = ccipca[["RepFrames_cur"]][clusters == cluster, ][i, ]

		# dest_dir = clust_dir
		# Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

		# dest_dir = paste(clust_dir, "/", cluster, sep = "")
		# Output_image(name = name, data = data, dest_dir = dest_dir, W = W, H = H)

		# video_file = video_files_cl[i]

		# dest_dir = clust_dir
		# system(paste("cp ", frame_dir, "/", video_file, ".jpg", " ", dest_dir, sep = ""))

		# dest_dir = paste(clust_dir, "/", cluster, sep = "")
		# system(paste("cp ", frame_dir, "/", video_file, ".jpg", " ", dest_dir, sep = ""))

		video_file = video_files_cl[i]
		video_file

		dest_dir = clust_dir
		require(rPython)
		setwd(master_dir)
		python.load("Remove_trap.py")
		python.assign("video_file", video_file)
		python.assign("frame_dir", frame_dir)
		python.assign("dest_dir", dest_dir)
		python.exec("
		import cv2
		import os
		os.chdir(frame_dir)
		frame = cv2.imread(video_file + '.jpg')
		frame = Remove_trap( frame )
		os.chdir(dest_dir)
		cv2.imwrite(video_file + '.jpg', frame)
		")

		dest_dir = paste(clust_dir, "/", cluster, sep = "")
		require(rPython)
		setwd(master_dir)
		python.load("Remove_trap.py")
		python.assign("video_file", video_file)
		python.assign("frame_dir", frame_dir)
		python.assign("dest_dir", dest_dir)
		python.exec("
		import cv2
		import os
		os.chdir(frame_dir)
		frame = cv2.imread(video_file + '.jpg')
		frame = Remove_trap( frame )
		os.chdir(dest_dir)
		cv2.imwrite(video_file + '.jpg', frame)
		")

	}
}

