# library(dendextend)
library(dendroextras)
library(plot3D)
library(rgl)
library(plyr)
library(stringr)
library(NbClust)
library(modeest)
library(scatterplot3d)
library(beepr)

posVar <- function(data) {
	out <- lapply(data, function(x) length(unique(x)))
	want <- which(out > 1)
	unlist(want)
}

# sets working direcory as data directory
setwd("/home/jim/Desktop/video_cluster/")

# apply pca for each set and trap video
i <- 3
rep_frames <- vector()
setwd("/home/jim/Desktop/video_cluster/video")
for(i in seq(1, length(list.files()), 1)) {

	setwd("/home/jim/Desktop/video_cluster/video")
	DFO2013_SiiiTjjj_GP <- str_match(sort(list.files())[i],'(.+)\\.MP4')[2]
	DFO2013_SiiiTjjj_GP
	setwd("/home/jim/Desktop/video_cluster/data/")

	setwd("/home/jim/Desktop/video_cluster/")
	system(paste("python 2_Extract_data.py"," ", DFO2013_SiiiTjjj_GP, ".MP4", sep = ""))
	setwd("/home/jim/Desktop/video_cluster/data/")

	na.omit(str_match(list.files(), paste(DFO2013_SiiiTjjj_GP, '.+', '_[a-z][a-z][a-z]\\.txt', sep = "")))

	DFO2013_SiiiTjjj_GP_Ffff <- str_match(list.files(), paste('(', DFO2013_SiiiTjjj_GP, '.+)', '_[a-z][a-z][a-z]\\.txt', sep = ""))[, 2]
	DFO2013_SiiiTjjj_GP_Ffff_u <- sort(unique(na.omit(DFO2013_SiiiTjjj_GP_Ffff)))

	cols.m <- data.frame()
	filename <- DFO2013_SiiiTjjj_GP_Ffff_u[i]
	for(filename in DFO2013_SiiiTjjj_GP_Ffff_u){

		filename

		getwd()

		red <- read.table(paste(filename, "_red.txt", sep = ""))
		red <- as.matrix(red, ncol = 1)
		red <- as.vector(red)

		gre <- read.table(paste(filename, "_gre.txt", sep = ""))
		gre <- as.matrix(gre, ncol = 1)
		gre <- as.vector(gre)

		blu <- read.table(paste(filename, "_blu.txt", sep = ""))
		blu <- as.matrix(blu, ncol = 1)
		blu <- as.vector(blu)

		col <- c(red, gre, blu)

		cols.m <- rbind(cols.m, col)

	}

	cols.m <- cols.m[, posVar(cols.m)]
	print(nrow(cols.m))

	# apply principal component analysis
	pr <- prcomp(x = cols.m
		,center=TRUE
		,scale=TRUE
		,retx=TRUE
		)

	# optimal number of cluster
	NbClust <- NbClust(data = pr$x[, c("PC1", "PC2", "PC3")], 
		method = 'complete', min.nc = 2, max.nc = nrow(pr$x) - 1,
		index = 'ch')

	fit <- hclust(dist(pr$x[, c("PC1", "PC2", "PC2")]), method = 'ward.D2')
	cl <- cutree(fit, k = NbClust$Best.nc[1])
	cl <- cutree(fit, k = 5)
	cl <- as.numeric(cl)
	length(cl)
	mfv(cl)

	rep_frame <- DFO2013_SiiiTjjj_GP_Ffff_u[cl == mfv(cl)][1]
	rep_frame
	rep_frames <- c(rep_frames, rep_frame)

	# mode_pc1 <- pr$x[cl == mfv(cl)[1], "PC1"][1]
	# mode_pc2 <- pr$x[cl == mfv(cl)[1], "PC2"][1]
	# mode_pc3 <- pr$x[cl == mfv(cl)[1], "PC3"][1]

	# # first two components
	# plot(pr$x[,c("PC1")], pr$x[,c("PC2")])
	# points(mode_pc1, mode_pc2, col = 'red')

	# # install rgl library
	# plot3d(pr$x[, c("PC1")], pr$x[, c("PC2")], pr$x[, c("PC3")])

	# # scatter 3d
	# sc <- scatterplot3d(pr$x[, c("PC1", "PC2", "PC2")])
	# sc$points3d(mode_pc1, mode_pc2, mode_pc3, col = 'red')

}

setwd("/home/jim/Desktop/video_cluster/")
rep_frames
write.csv(rep_frames, "rep_frames.csv")

rep_frames <- read.csv("rep_frames.csv")
rep_frames <- as.vector(rep_frames[, 2])
rep_frames

beep()

# ----------------------------------------------------------------

cols.m <- data.frame()
for(DFO2013_SiiiTjjj_GP_Ffff_u in rep_frames){

	red <- read.table(paste(DFO2013_SiiiTjjj_GP_Ffff_u, "_red.txt", sep = ""))
	red <- as.matrix(red, ncol = 1)
	red <- as.vector(red)

	gre <- read.table(paste(DFO2013_SiiiTjjj_GP_Ffff_u, "_gre.txt", sep = ""))
	gre <- as.matrix(gre, ncol = 1)
	gre <- as.vector(gre)

	blu <- read.table(paste(DFO2013_SiiiTjjj_GP_Ffff_u, "_blu.txt", sep = ""))
	blu <- as.matrix(blu, ncol = 1)
	blu <- as.vector(blu)

	col <- c(red, gre, blu)

	cols.m <- rbind(cols.m, col)

}

cols.m <- cols.m[, posVar(cols.m)]

# apply principal component analysis
pr <- prcomp(x = cols.m
	,center=TRUE
	,scale=TRUE
	,retx=TRUE
	)

# biplot of pca
# biplot(pr)

# summary
summary(pr)

# screeplot
screeplot(pr)

image_pc1 <- pr$rotation[, "PC1"]
image_pc2 <- pr$rotation[, "PC2"]
image_pc3 <- pr$rotation[, "PC3"]

# first two components
plot(pr$x[,c("PC1")], pr$x[,c("PC2")])

# install rgl library
plot3d(pr$x[, c("PC1")], pr$x[, c("PC2")], pr$x[, c("PC3")])

# pca matrix, first two principal components
x <- pr$x[,seq(1, 10, 1)]
x <- as.data.frame(x)
row.names(x) <- rep_frames

NbClust <- NbClust(data = x, min.nc = 2, max.nc = nrow(x) - 2,
 method = 'kmeans')
NbClust$Best.nc[1]

# ----------------------------------------------------
# hclust

# define distance and number of clusters
dist <- dist(x, method = 'euclidean')
k <- 9

# apply clustering algorithm
fit <- hclust(dist, method = "ward.D2") 

# pull out cluster names
sl <- slice(fit,k=k)
sl <- as.matrix(sl)
sl <- as.data.frame(sl)
sl$rn <- row.names(sl)
sl <- sl[order(sl$rn),]

# define cluster names
cluster <- sl$V1

# create png of dendogram
png("dendrogram.png", height=800, width=800)
fit_col <- colour_clusters(as.dendrogram(fit), k=k, groupLabels = TRUE)
plot(fit_col)
rect.dendrogram(fit_col, k = k, border = "red")
dev.off()

x$cluster <- cluster

# --------------------------------------------------

# define distance and number of clusters
dist <- dist(x, method = 'euclidean')
k <- 9

# apply clustering algorithm
km <- kmeans(dist(x, method = 'euclidean'), centers = k) 
head(km)

x$cluster <- km$cluster


# ---------------------------------------------------

setwd('/home/jim/Desktop/video_cluster')
write.csv(x, "x.csv")
setwd('/home/jim/Desktop/video_cluster/data')

setwd('/home/jim/Desktop/video_cluster/frames')
rep_frame <- rep_frames[1]
rep_frame
cl <- unique(x$cluster)[1]

system(paste('rm -r ', getwd(), '/', 'rep_frames', sep = ""))
system(paste('mkdir ', getwd(), '/', 'rep_frames', sep = ""))

for(cl in unique(x$cluster)){

	system(paste('rm -r ', getwd(), '/', cl, sep = ""))
	system(paste('mkdir ', getwd(), '/', cl, sep = ""))

	rep_frames_c <- rep_frames[x$cluster == cl]
	for(rep_frame in rep_frames_c){
		system(paste('cp ', getwd(), '/', rep_frame, '.jpg', ' ', 
			getwd(), '/', cl, sep = ''))
		system(paste('cp ', getwd(), '/', rep_frame, '.jpg', ' ', 
			getwd(), '/', 'rep_frames', sep = ''))
	}
}

setwd('/home/jim/Desktop/video_cluster/data')

summ_x <- ddply(x, .(cluster), summarize, 
	meanPC1 = mean(PC1),
	meanPC2 = mean(PC2),
	meanPC3 = mean(PC3))

summ_x

# ----------------------------------------
# eigen traps

setwd('/home/jim/Desktop/video_cluster/')

image_pc1 <- pr$rotation[, "PC1"]
image_pc2 <- pr$rotation[, "PC2"]
image_pc3 <- pr$rotation[, "PC3"]

m <- max(c(image_pc1, image_pc2, image_pc3))
m <- abs(m)
m

pr_rotation <- floor(255*pr$rotation/m)
head(pr_rotation)

# image_pc1 <- floor(255*pr_rotation[, "PC1"]/m)
# image_pc2 <- floor(255*pr_rotation[, "PC2"]/m)
# image_pc3 <- floor(255*pr_rotation[, "PC3"]/m)

n <- length(image_pc1)/3
n

# red

image_pc1_red <- pr_rotation[seq(1, n, 1), "PC1"]
image_pc2_red <- pr_rotation[seq(1, n, 1), "PC2"]
image_pc3_red <- pr_rotation[seq(1, n, 1), "PC3"]

image_pc1_red <- matrix(image_pc1_red, ncol = 192, nrow = 108)
image_pc2_red <- matrix(image_pc2_red, ncol = 192, nrow = 108)
image_pc3_red <- matrix(image_pc3_red, ncol = 192, nrow = 108)

# green

image_pc1_gre <- pr_rotation[seq(n + 1, 2*n, 1), "PC1"]
image_pc2_gre <- pr_rotation[seq(n + 1, 2*n, 1), "PC2"]
image_pc3_gre <- pr_rotation[seq(n + 1, 2*n, 1), "PC3"]

image_pc1_gre <- matrix(image_pc1_gre, ncol = 192, nrow = 108)
image_pc2_gre <- matrix(image_pc2_gre, ncol = 192, nrow = 108)
image_pc3_gre <- matrix(image_pc3_gre, ncol = 192, nrow = 108)

# blue

image_pc1_blu <- pr_rotation[seq(2*n + 1, 3*n, 1), "PC1"]
image_pc2_blu <- pr_rotation[seq(2*n + 1, 3*n, 1), "PC2"]
image_pc3_blu <- pr_rotation[seq(2*n + 1, 3*n, 1), "PC3"]

image_pc1_blu <- matrix(image_pc1_blu, ncol = 192, nrow = 108)
image_pc2_blu <- matrix(image_pc2_blu, ncol = 192, nrow = 108)
image_pc3_blu <- matrix(image_pc3_blu, ncol = 192, nrow = 108)

# output

write.table(image_pc1_red, "image_pc1_red.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc2_red, "image_pc2_red.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc3_red, "image_pc3_red.txt", col.names = FALSE, row.names = FALSE)

write.table(image_pc1_gre, "image_pc1_gre.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc2_gre, "image_pc2_gre.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc3_gre, "image_pc3_gre.txt", col.names = FALSE, row.names = FALSE)

write.table(image_pc1_blu, "image_pc1_blu.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc2_blu, "image_pc2_blu.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc3_blu, "image_pc3_blu.txt", col.names = FALSE, row.names = FALSE)
