setwd(master_dir)
load(paste('pca_audio', year, '.RData', sep = ''))

prx <- pca[["prx"]]
prx <- as.data.frame(prx)
row.names(prx) <- pca[["audio_files"]]

# set number of cluster
k <- 15

# define distance and number of clusters
dist <- dist(prx, method = 'euclidean')

# apply clustering algorithm
fit <- hclust(dist, method = "average") 

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
png("Audio_dendrogram_hclust.png")
fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE, col = sort(unique(clusters)))
plot(fit_col)
require(dendextend)
# rect.dendrogram(fit_col, k = k, border = "red")
dev.off()

memb <- cutree(fit, k = k)
cent <- NULL
for(cl in 1:k){
	cent <- rbind(cent, colMeans(prx[memb == cl, ]))
}

setwd(master_dir)
png("Audio_cluster_hclust.png")
plot(prx[, 1:2], col = clusters)
points(cent[, 1:2], pch = 16)
text(cent[, 1:2], labels = unique(clusters), pos = 3)
dev.off()

fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE, col = sort(unique(clusters)))
plot(fit_col)
require(dendextend)
# rect.dendrogram(fit_col, k = k, border = "red")
dev.new()

plot(prx[, 1:2], col = clusters)
points(cent[, 1:2], pch = 16)
text(cent[, 1:2], labels = unique(clusters), pos = 3)

# ----------------------------------------------------
# kmeans

# setwd(master_dir)
# load(paste('pca_audio', year, '.RData', sep = ''))

# prx <- pca[["prx"]]

# k <- 10
# km <- kmeans(prx, centers = k)
# clusters <- km$cluster
# clusters

# plot(prx[, 1:2], col = clusters)
# points(km$centers[, c(1, 2)], pch = 16)
# text(km$centers[, 1:2], labels = sort(unique(clusters)), pos = 3)

# setwd(master_dir)
# png("Audio_cluster_kmeans.png")
# plot(prx[, 1:2], col = clusters)
# points(km$centers[, c(1, 2)], pch = 16)
# text(km$centers[, 1:2], labels = sort(unique(clusters)), pos = 3)
# dev.off()

# ---------------------------------------------

# extracts audio from video files and copies it to audio folder
setwd(audio_dir)
for(cluster in unique(clusters)) {
	system(paste("rm -r ", audio_dir, "/", cluster, sep = ""))
	system(paste("mkdir ", audio_dir, "/", cluster, sep = ""))

	audio_files_cl <- audio_files[cluster == clusters]

	for(audio_file in audio_files_cl) {
		system(command = paste("cp ", audio_dir, "/", audio_file, " ", audio_dir, "/", cluster, sep = ""))
	}
}

pca[["clusters"]] <- clusters

# write pca list to RData
setwd(master_dir)
save(pca, file = paste('pca_audio', year, '.RData', sep = ''))

