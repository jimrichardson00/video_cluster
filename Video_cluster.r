# Input:
# - k (number of groups to be output in clustering)
# Output:
# - Loads ccipca data from .RData file
# - Subsets to frames stored in ccipca data but not stored in (clust_dir)/cur/ImageClarity_validated
# - For these files, loads the reduced dimension data set.
# - Runs a hireachical clustering algorithm on reduced dimesion data set. Details are as follows; k = 40 groups (set in Video_master.r), usual euclidean distance used for dissimilarity matrix, ward.D2 used for agglomeration method.
# - Clustering algorithm groups together frames which are close together in this space (frames look similar).
# - Copies all jpgs into (clust_dir)/new, and for each cluster group i, copies jpgs in group i to folder (clust_dir)/new/i.

# sets frames as images contained in frame_dir
require(stringr)
frames <- str_match(list.files(frame_dir, pattern = ".jpg"), "(.+)\\.jpg")[, 2]

# sets frames_cur as images contained in (clust_dir)/cur/ImageClarity_validated
length(list.files(paste(clust_dir, "/cur/ImageClarity_validated", sep = ""), pattern = ".jpg")) > 0
if(length(list.files(paste(clust_dir, "/cur/ImageClarity_validated", sep = ""), pattern = ".jpg")) > 0) {
  frames_cur <- na.omit(str_match(list.files(paste(clust_dir, "/cur/ImageClarity_validated", sep = ""), pattern = ".jpg"), "(.+)\\.jpg")[, 2])
} else {
  frames_cur <- vector()
}

# sets frames_new as images contained in frames, but not contained in frames_cur
frames_new <- frames[!(frames %in% frames_cur)]
frames_new

# if there are more than 2 frames in frames_new, runs clustering on them
if(length(frames_new) >= 2) {

	# if there are less than k frames in frames_new, sets k = length(frames_new)
  if(k > length(frames_new)) {
    k <- length(frames_new)
  }

  # loads ccipca data, subsets reduced dimension data set prx to frames_new
  setwd(master_dir)
  prx <- read.table(paste("prx", year, ".txt", sep = ""))
  prx <- as.data.frame(prx)
  video_files_cur <- as.vector(read.table(paste("video_files_cur", year, ".txt", sep = ""))[, 1])
  row.names(prx) <- video_files_cur
  prx <- prx[video_files_cur %in% frames_new, ]

  # ------------------------------------------------

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

  # set colours as rainbow of length k
  colours <- rainbow(k)
  colours

  # calculates centriod of each group
  memb <- cutree(fit, k = k)
  cent <- NULL
  for(cl in 1:k){
    cent <- rbind(cent, colMeans(prx[memb == cl, , drop = FALSE]))
  }

  # plots dendogram of clustering, writes to .jpeg
  jpeg("Video_hclust_dendrogram.jpeg")
  fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE, col = colours)
  plot(fit_col)
  require(dendextend)
  rect.dendrogram(fit_col, k = k, border = "red")
  dev.off()

  # plots points and cluster centriods projected onto Princ1, Princ2, writes to .jpeg
  jpeg("Video_cluster_hclust.jpeg")
  plot(prx[, 1:2], col = colours)
  points(cent[, 1:2], pch = 16)
  text(cent[, 1:2], labels = unique(clusters), pos = 3)
  dev.off()

  # plots dendogram of clustering, writes to .jpeg
  fit_col <- colour_clusters(fit, k = k, groupLabels = TRUE, col = colours)
  plot(fit_col)
  require(dendextend)
  # rect.dendrogram(fit_col, k = k, border = "red")
  dev.new()

  # plots points and cluster centriods projected onto Princ1, Princ2, writes to .jpeg
  plot(prx[, 1:2], col = colours)
  points(cent[, 1:2], pch = 16)
  text(cent[, 1:2], labels = unique(clusters), pos = 3)

  # -------------------------------------------------# applies kmeans clustering

  # # loads ccipca data, subsets reduced dimension data set prx to frames_new
  # setwd(master_dir)
  # load(paste("ccipca_video", year, ".RData", sep = ""))
  # prx <- ccipca[["prx"]]
  # prx <- as.data.frame(prx)
  # video_files_cur <- ccipca[["video_files_cur"]]
  # row.names(prx) <- video_files_cur
  # prx <- prx[ccipca[["video_files_cur"]] %in% frames_new, ]

  # # apply clustering algorithm
  # km <- kmeans(ccipca[["prx"]], centers = k) 
  # clusters <- km$cluster

  # setwd(master_dir)
  # jpeg("Video_cluster_kmeans.jpeg")
  # plot(ccipca[["prx"]][, 1], ccipca[["prx"]][, 2], col = clusters)
  # points(km$centers[, c(1, 2)], pch = 16)
  # text(km$centers[, 1:2], labels = sort(unique(clusters)), pos = 3)
  # dev.off()

  # -----------------------------------------------
	# Copies all jpgs into (clust_dir)/new, and for each cluster group i, copies jpgs in group i to folder (clust_dir)/new/i.

  # deletes (clust_dir)/new folder, then creates it
  system(paste('rm -r ', clust_dir, "/new", sep = ""))
  system(paste('mkdir ', clust_dir, "/new", sep = ""))

  # cycles through clusters
  for(cluster in sort(unique(clusters))) {

    print(paste("Cluster: ", cluster, sep = ""))

    # deletes cluster folder, then creates it (aviods overlap)
    system(paste("rm -r ", clust_dir, "/new/", cluster, sep = ""))
    system(paste("mkdir ", clust_dir, "/new/", cluster, sep = ""))

    # subsets to images within cluster
    video_files_cl <- row.names(prx)[clusters == cluster]
    video_files_cl

    # for each image in cluster i, copies to (cluster_dir)/new and (cluster_dir)/new/i
    for(i in seq(1, sum(clusters == cluster), 1)){

    	# sets video_file as ith frame in current cluster
      video_file = video_files_cl[i]
      video_file

      # copies video_file to (cluster_dir)/new
      dest_dir = paste(clust_dir, "/new", sep = "")
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

      # copies video_file to (cluster_dir)/new/i
      dest_dir = paste(clust_dir, "/new/", cluster, sep = "")
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
}

