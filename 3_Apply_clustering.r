library(dendextend)
library(dendroextras)
library(plot3D)
library(rgl)
library(plyr)
library(stringr)
library(NbClust)
library(modeest)
library(scatterplot3d)
library(beepr)
library(rPython)

posVar <- function(data) {
	out <- lapply(data, function(x) length(unique(x)))
	want <- which(out > 1)
	unlist(want)
}

extract_data <- function(DFO2013_SiiiTjjj_GP) {
	setwd("/home/jim/Desktop/video_cluster/")
	system(paste("python 2_Extract_data.py ", DFO2013_SiiiTjjj_GP, ".MP4", sep = ""))
}

rep_frm <- function(DFO2013_SiiiTjjj_GP_Ffff_u) {

	setwd("/home/jim/Desktop/video_cluster/data/")

	cols.m <- data.frame()
	for(filename in DFO2013_SiiiTjjj_GP_Ffff_u){

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
	cl <- as.numeric(cl)

	rep_frame <- DFO2013_SiiiTjjj_GP_Ffff_u[cl == mfv(cl)][1]
	return(rep_frame)
}

# ----------------------------------------------------------------
# copies videos into one folder with labels

from_dirs <- c('/home/jim/Desktop/DFO Survey - October 2013 - copy/october 2013 video')
from_idxs <- c("DF02013")
to_dir <-  '/home/jim/Desktop/video_cluster/video'

for(from_dir in from_dirs){
	setwd("/home/jim/Desktop/video_cluster/")
	system("python 1_Rename_video.py ",
		"'", from_idx, "'", " ", 
		"'", from_dir, "'", " ", 
		"'", to_dir, "'", sep = "")
}

# ----------------------------------------------------------------
# extracts data from each video and pulls out representative frame

# rep_frames_cur = current rep frames
if(file.exists('rep_frames_cur.txt') == TRUE){
	rep_frames_cur <- read.table("rep_frames_cur.txt")
	rep_frames_cur <- as.vector(rep_frames_cur$V1)
	rep_frames_cur
} else {
	rep_frames_cur <- vector()
}

# rep_frame_vid = videos that have representative frames
# videos that have representative frames
rep_frames_vid <- unique(na.omit(str_match(rep_frames_cur, '(.+)_F.+')[, 2]))

# list of videos to cluster on
setwd("/home/jim/Desktop/video_cluster/video")
DFO2013_SiiiTjjj_GP_fns <- na.omit(str_match(list.files(), '(.+)\\.MP4'))[, 2]
DFO2013_SiiiTjjj_GP_fns

# new videos to calculate the representative frames for
video_new <- DFO2013_SiiiTjjj_GP_fns[!(DFO2013_SiiiTjjj_GP_fns %in% rep_frames_vid)]

# calculates new rep frames
rep_frames_new <- vector()
if(length(video_new) > 0){
	for(DFO2013_SiiiTjjj_GP in video_new){

		extract_data(DFO2013_SiiiTjjj_GP)
		
		setwd("/home/jim/Desktop/video_cluster/data")
		DFO2013_SiiiTjjj_GP_Ffff <- str_match(list.files(), paste('(', DFO2013_SiiiTjjj_GP, '.+)', '_[a-z][a-z][a-z]\\.txt', sep = ""))[, 2]
		DFO2013_SiiiTjjj_GP_Ffff_u <- sort(unique(na.omit(DFO2013_SiiiTjjj_GP_Ffff)))

		rep_frame <- rep_frm(DFO2013_SiiiTjjj_GP_Ffff_u)
		rep_frames_new <- c(rep_frames_new, rep_frame)
	} 
}

setwd("/home/jim/Desktop/video_cluster/")
write(rep_frames_cur, "rep_frames_cur.txt")

rep_frames_cur <- read.table("rep_frames_cur.txt", )
rep_frames_cur <- as.vector(rep_frames_cur$V1)
rep_frames_cur

# ----------------------------------------------------------------
# clusters on rep_frames

# rep_frames_new = new representative frames to add
# rep_frames_cur = current representative frames that are already in the pca

rep_frames_add <- rep

# ----------------------------------------------------

m = 10
n = 1000
p = 3
python.assign("m", 10)
python.assign("n", 1000)
python.assign("p", 3)

mean <- read.table('mean.txt')
mean <- as.matrix(mean)
colnames(mean) = NULL
python.assign("mean", mean)
python.exec('
	import numpy
	mean =  numpy.matrix(mean)')

covariance <- read.table('covariance.txt')
covariance <- as.matrix(covariance)
colnames(covariance) = NULL
python.assign("covariance", covariance)
python.exec('
	import numpy
	covariance =  numpy.matrix(covariance)')

eigenvectors <- read.table('eigenvectors.txt')
eigenvectors <- as.matrix(eigenvectors)
colnames(eigenvectors) = NULL
python.assign("eigenvectors", eigenvectors)
python.exec('print eigenvectors')
python.exec('
	import numpy
	eigenvectors =  numpy.matrix(eigenvectors)')

eigenvalues <- read.table('eigenvalues.txt')
eigenvalues <- as.matrix(eigenvalues)
colnames(eigenvalues) = NULL
python.assign("eigenvalues", eigenvalues)
python.exec('print eigenvalues')
python.exec('
	import numpy
	eigenvalues =  numpy.matrix(eigenvalues)')

i <- 1
for(i in seq(1, 10, 1)){
	python.assign("x", obs[i, ])
	python.exec('
	import numpy
	x =  numpy.matrix(x)
	x = x.transpose()')
	python.load("IPCA.py")
}

python.exec("
	import numpy
	numpy.savetxt('eigenvectors.txt', eigenvectors)")


eigenvectors <- read.table('eigenvectors.txt')
eigenvectors <- as.matrix(eigenvectors)
colnames(eigenvectors) = NULL
eigenvectors

t(t(eigenvectors) %*% t(obs[1:100, ]))

# ----------------------------------------------------

setwd("/home/jim/Desktop/video_cluster/data/")
for(DFO2013_SiiiTjjj_GP_Ffff_u in rep_frames_new){

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

	col <- c(1, 2, 3)

	python.assign("col", col)

	setwd("/home/jim/Desktop/video_cluster/")

	m <- 192*108
    n <- 
    p <- 2


}




cols.m <- cols.m[, posVar(cols.m)]

# apply principal component analysis
pr <- prcomp(x = cols.m
	,center=TRUE
	,scale=FALSE
	,retx=TRUE
	)

# biplot of pca
# biplot(pr)

# summary
summary(pr)

# screeplot
screeplot(pr)

# first two components
plot(pr$x[,c("PC1")], pr$x[,c("PC2")])

# install rgl library
plot3d(pr$x[, c("PC1")], pr$x[, c("PC2")], pr$x[, c("PC3")])

# pca matrix, first two principal components
x <- pr$x[,seq(1, 68, 1)]
x <- as.data.frame(x)
row.names(x) <- rep_frames

NbClust <- NbClust(data = x, min.nc = 2, max.nc = nrow(x) - 2,
 method = 'kmeans')
NbClust$Best.nc[1]

# ----------------------------------------------------
# applies hclust clustering

# define distance and number of clusters
dist <- dist(x, method = 'euclidean')
k <- 31

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
# applies kmeans clustering

# # define distance and number of clusters
# dist <- dist(x, method = 'euclidean')
# k <- 31

# # apply clustering algorithm
# km <- kmeans(dist(x, method = 'euclidean'), centers = k) 
# head(km)

# x$cluster <- km$cluster

# ---------------------------------------------------
# copies frames into cluster folders, and into rep_frames folder

# writes data to .csv
setwd('/home/jim/Desktop/video_cluster')
write.csv(x, "x.csv")
setwd('/home/jim/Desktop/video_cluster/data')

# deletes rep_frames folder, then creates it (aviods overlap)
setwd('/home/jim/Desktop/video_cluster/frames')
system(paste('rm -r ', getwd(), '/', 'rep_frames', sep = ""))
system(paste('mkdir ', getwd(), '/', 'rep_frames', sep = ""))

# cycles through clusters
for(cl in unique(x$cluster)){

	# deletes cluster folder, then creates it (aviods overlap)
	system(paste('rm -r ', getwd(), '/', cl, sep = ""))
	system(paste('mkdir ', getwd(), '/', cl, sep = ""))

	# subsets to rep_frames in cluster
	rep_frames_c <- rep_frames[x$cluster == cl]
	for(rep_frame in rep_frames_c){

		# copies frame to cluster folder
		system(paste('cp ', getwd(), '/', rep_frame, '.jpg', ' ', 
			getwd(), '/', cl, sep = ''))
		
		# copies frame to rep_frames folder
		system(paste('cp ', getwd(), '/', rep_frame, '.jpg', ' ', 
			getwd(), '/', 'rep_frames', sep = ''))
	}
}

# ----------------------------------------
# eigen traps
n.pc <- 3

setwd('/home/jim/Desktop/video_cluster/')

m <- max(pr$rotation[, seq(1, 3, 1)])
m <- abs(m)
m

pr_rotation <- floor(255*pr$rotation/m)

n <- nrow(pr$rotation)/3
n

for(pc in seq(1, n.pc, 1)){

	# red
	image_pc_red <- pr_rotation[seq(1, n, 1), pc]
	image_pc_red <- matrix(image_pc_red, ncol = 192, nrow = 108)
	write.table(image_pc_red, paste("image_pc", pc, "_red.txt", sep = ""), col.names = FALSE, row.names = FALSE)

	# green
	image_pc_gre <- pr_rotation[seq(n + 1, 2*n, 1), pc]
	image_pc_gre <- matrix(image_pc_gre, ncol = 192, nrow = 108)
	write.table(image_pc_gre, paste("image_pc", pc, "_gre.txt", sep = ""), col.names = FALSE, row.names = FALSE)

	# blue
	image_pc_blu <- pr_rotation[seq(2*n + 1, 3*n, 1), pc]
	image_pc_blu <- matrix(image_pc_blu, ncol = 192, nrow = 108)
	write.table(image_pc_blu, paste("image_pc", pc, "_blu.txt", sep = ""), col.names = FALSE, row.names = FALSE)

}

system(paste("python 4_Output_prcomp.py ", n.pc + 1, sep = ""))
