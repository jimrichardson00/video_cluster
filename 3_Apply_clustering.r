library(dendextend)
library(dendroextras)
library(plot3D)
library(rgl)
library(plyr)
library(stringr)

# sets working direcory as data directory
setwd("/home/jim/Desktop/pca_video/data/")

filenames_col <- list.files()
head(filenames_col)
length(filenames_col)

filenames <- str_match(filenames_col, '(.+)_[a-z][a-z][a-z]\\.txt')[, 2]
head(filenames)
length(filenames)

filenames <- sort(unique(filenames))
length(filenames)

cols.m <- data.frame()
for(filename in filenames) {

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

# apply principal component analysis
pr <- prcomp(x = cols.m
	,center=TRUE
	,scale=TRUE
	,retx=TRUE
	)

# biplot of pca
biplot(pr)

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
x <- pr$x[,c("PC1", "PC2", "PC3")]
x <- as.data.frame(x)
row.names(x) <- filenames

# define distance and number of clusters
dist <- dist(x, method='euclidean')
k <- 5

# apply clustering algorithm
fit <- hclust(dist, method="ward.D2") 

# pull out cluster names
sl <- slice(fit,k=k)
sl <- as.matrix(sl)
sl <- as.data.frame(sl)
sl$rn <- row.names(sl)
sl <- sl[order(sl$rn),]

# define cluster names
cluster <- sl$V1

# create png of dendogram
png("dendrogram.png",height=800,width=800)
fit_col <- colour_clusters(as.dendrogram(fit) ,k=k,groupLabels=TRUE)
plot(fit_col)
rect.dendrogram(fit_col,k=k, border="red")
dev.off()

head(x)
head(cluster)

x$cluster <- cluster

summ_x <- ddply(x, .(cluster), summarize, 
	meanPC1 = mean(PC1),
	meanPC2 = mean(PC2),
	meanPC3 = mean(PC3))

summ_x

# ----------------------------------------
# eigen traps

image_pc1 <- pr$rotation[, "PC1"]
image_pc2 <- pr$rotation[, "PC2"]
image_pc3 <- pr$rotation[, "PC3"]

m <- max(c(image_pc1, image_pc2, image_pc3))
m

image_pc1 <- floor(255*pr$rotation[, "PC1"]/m)
image_pc2 <- floor(255*pr$rotation[, "PC2"]/m)
image_pc3 <- floor(255*pr$rotation[, "PC3"]/m)

n <- length(image_pc1)/3
n

image_pc1_red <- pr$rotation[seq(1, n, 1), "PC1"]
image_pc2_red <- pr$rotation[seq(1, n, 1), "PC2"]
image_pc3_red <- pr$rotation[seq(1, n, 1), "PC3"]

image_pc1_gre <- pr$rotation[seq(n + 1, 2*n, 1), "PC1"]
image_pc2_gre <- pr$rotation[seq(n + 1, 2*n, 1), "PC2"]
image_pc3_gre <- pr$rotation[seq(n + 1, 2*n, 1), "PC3"]

image_pc1_blu <- pr$rotation[seq(2*n + 1, 3*n, 1), "PC1"]
image_pc2_blu <- pr$rotation[seq(2*n + 1, 3*n, 1), "PC2"]
image_pc3_blu <- pr$rotation[seq(2*n + 1, 3*n, 1), "PC3"]

write.table(image_pc1_red, "image_pc1_red.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc2_red, "image_pc2_red.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc3_red, "image_pc3_red.txt", col.names = FALSE, row.names = FALSE)

write.table(image_pc1_gre, "image_pc1_gre.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc2_gre, "image_pc2_gre.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc3_gre, "image_pc3_gre.txt", col.names = FALSE, row.names = FALSE)

write.table(image_pc1_blu, "image_pc1_blu.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc2_blu, "image_pc2_blu.txt", col.names = FALSE, row.names = FALSE)
write.table(image_pc3_blu, "image_pc3_blu.txt", col.names = FALSE, row.names = FALSE)
