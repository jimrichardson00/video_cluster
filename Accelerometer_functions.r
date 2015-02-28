setwd(master_dir)

AllCameraData <- read.csv("AllCameraData.CSV")

Set014Trap014RAW <- read.csv("Set014Trap014RAW.csv", skip = 20)
Set014Trap014RAW <- na.omit(Set014Trap014RAW)
N = nrow(Set014Trap014RAW)
# Set014Trap014RAW <- Set014Trap014RAW[seq(1, N, length = 100000), ]
Set014Trap014RAW$VSum <- sqrt(Set014Trap014RAW[, 1]^2 + Set014Trap014RAW[, 2]^2 + Set014Trap014RAW[, 3]^2)
Set014Trap014RAW$VSum_l <- predict(smooth.spline(x = seq(1, N, 1), y = Set014Trap014RAW$VSum, spar = 0.6213992), x = seq(1, N, 1))$y
names(Set014Trap014RAW) <- c("AccX", "AccY", "AccZ", "VSum", "VSum_l")

Set014Trap014RAW$VelX <- cumsum(Set014Trap014RAW$AccX)*(1/100)
Set014Trap014RAW$VelX <- Set014Trap014RAW$VelX - median(Set014Trap014RAW[seq(floor(0.5*N), N, 1), ]$VelX)

Set014Trap014RAW$VelY <- cumsum(Set014Trap014RAW$AccY)*(1/100)
Set014Trap014RAW$VelY <- Set014Trap014RAW$VelY - median(Set014Trap014RAW[seq(floor(0.5*N), N, 1), ]$VelY)

Set014Trap014RAW$VelZ <- cumsum(Set014Trap014RAW$AccZ)*(1/100)
Set014Trap014RAW$VelZ <- Set014Trap014RAW$VelZ - median(Set014Trap014RAW[seq(floor(0.5*N), N, 1), ]$VelZ)

plot(Set014Trap014RAW$AccX[seq(1, N, length = 100000)], type = 'l')
abline(h = median(Set014Trap014RAW$AccX), col = 'blue')

plot(Set014Trap014RAW$AccY[seq(1, N, length = 100000)], type = 'l')
abline(h = median(Set014Trap014RAW$AccY), col = 'blue')

plot(Set014Trap014RAW$AccZ[seq(1, N, length = 100000)], type = 'l')
abline(h = median(Set014Trap014RAW$AccZ), col = 'blue')

plot(Set014Trap014RAW$VSum[seq(1, N, length = 100000)], type = 'l')
abline(h = median(Set014Trap014RAW$VSum), col = 'blue')

# 100 seconds, 1.6 mins
data = Set014Trap014RAW[1:1000000, ]

data$AccX <- data$AccX - median(Set014Trap014RAW[1000000:5000000, ]$AccX) 
data$AccY <- data$AccY - median(Set014Trap014RAW[1000000:5000000, ]$AccY) 
data$AccZ <- data$AccZ - median(Set014Trap014RAW[1000000:5000000, ]$AccZ)  
data$VSum <- data$VSum - median(Set014Trap014RAW[1000000:5000000, ]$VSum) 

data$AccX <- data$AccX - median(data$AccX) 
data$AccY <- data$AccY - median(data$AccY) 
data$AccZ <- data$AccZ - median(data$AccZ)  
data$VSum <- data$VSum - median(data$VSum) 

data$AccX_l <- smooth.spline(data$AccX)$y
data$AccY_l <- smooth.spline(data$AccY)$y
data$AccZ_l <- smooth.spline(data$AccZ)$y 
data$VSum_l <- smooth.spline(data$VSum)$y 

data$VelX <- cumsum(data$AccX)*(1/100)
data$VelY <- cumsum(data$AccY)*(1/100)
data$VelZ <- cumsum(data$AccZ)*(1/100)

data$VelX <- data$VelX - median(data$VelX) 
data$VelY <- data$VelY - median(data$VelY) 
data$VelZ <- data$VelZ - median(data$VelZ)  


data$PosX <- cumsum(data$VelX)*(1/100)
data$PosY <- cumsum(data$VelY)*(1/100)
data$PosZ <- cumsum(data$VelZ)*(1/100)

plot(data$VSum, type = 'l')
lines(data$VSum_l, col = 'red')
abline(h = median(Set014Trap014RAW[1000000:5000000, ]$VSum), col = 'blue')
abline(h = 0, col = 'green')

plot(data$AccX, type = 'l')
lines(data$AccX_l, col = 'red')
abline(h = median(Set014Trap014RAW[1000000:5000000, ]$AccX), col = 'blue')
dev.new()

plot(data$VelX, type = 'l')
dev.new()

plot(data$PosX, type = 'l')
dev.new()

data[seq(which.max(data$AccX) - 10, which.max(data$AccX) + 10, 1), ]

require(rgl)
plot3d(data[, c("VelX", "VelY", "VelZ")])
plot3d(data[, c("AccX", "AccY", "AccZ")])

plot(data$VSum, type = 'l')
lines(smooth.spline(data$VSum)$y, type = 'l', col = 'red')
smooth.spline(data$VSum)

plot(smooth.spline(data$VSum - median(data$VSum))$y, type = 'l')

plot(cumsum(smooth.spline(data$VSum - median(data$VSum))$y)*(1/100), type = 'l')




plot(data$VSum[1:10000] - smooth.spline(data$VSum[1:10000])$y, type = 'l')


plot(data$VSum[seq(3000000, 3000000 + 10000, 1)] - smooth.spline(data$VSum[seq(3000000, 3000000 + 10000, 1)])$y, type = 'l')

plot(data$VSum[seq(3000000, 3000000 + 10000, 1)], type = 'l')
lines(smooth.spline(data$VSum[seq(3000000, 3000000 + 10000, 1)])$y, type = 'l', col = 'red')

VSum_l = smooth.spline(data$VSum[seq(3000000, 3000000 + 10000, 1)])$y

plot(VSum_l, type = 'l')

require(signal)

specgram(data$VSum[1:10000], Fs = 100)
specgram(data$VSum[seq(3000000, 3000000 + 10000, 1)], Fs = 100)

plot(data$VSum[seq(3000000, 3000000 + 10000, 1)], type = 'l')
lines(smooth.spline(data$VSum[seq(3000000, 3000000 + 10000, 1)], spar = 0.8), type = 'l', col = 'red', pch = 16)
smooth.spline(data$VSum[seq(3000000, 3000000 + 10000, 1)])
dev.new()

plot(data$VSum[seq(0, 0 + 10000, 1)], type = 'l')
lines(data$VSum_l[seq(0, 0 + 10000, 1)], type = 'l', col = 'red', pch = 16)

plot(data$VSum_l)

data$VSum_l[seq(3000000, 3000000 + 10000, 1)]

pdf("VSum.pdf")
plot(data$VSum, type = 'l')
dev.off()

plot(data$VSum[80000:90000], type = 'l')
dev.new()
plot(data$VSum[70000:80000], type = 'l')
dev.new()
plot(data$VSum[60000:70000], type = 'l')
dev.new()
plot(data$VSum[50000:60000], type = 'l')
dev.new()
plot(data$VSum[40000:50000], type = 'l')
dev.new()
plot(data$VSum[30000:40000], type = 'l')
dev.new()
plot(data$VSum[20000:30000], type = 'l')
dev.new()
plot(data$VSum[10000:20000], type = 'l')
dev.new()
plot(data$VSum[0:10000], type = 'l')

VSum = 9.8*Set014Trap014RAW$VSum

smth <- smooth.spline(Set014Trap014RAW$VSum)
plot(Set014Trap014RAW$VSum - median(Set014Trap014RAW$VSum), type = 'l')
lines(predict(smth)$y - median(predict(smth)$y), col = 'red', pch = 16)

Set014Trap014RAW$velV <- cumsum(Set014Trap014RAW$VSum - median(Set014Trap014RAW$VSum))*(1/100)
plot(Set014Trap014RAW$velV, type = 'l')

Set014Trap014RAW$posV <- cumsum(Set014Trap014RAW$velV - median(Set014Trap014RAW$velV))*(1/100)

data = Set014Trap014RAW

nrow(data)

plot(Set014Trap014RAW$posV)

data

accel3d <- function (n = 20) 
{
  for (i in seq(1,nrow(data),length=n)) {

  	print(i)

    ani.options(interval=0.01/n)
   # dev.hold()

  	xlim <- range(data$AccX/data$VSum,na.rm=TRUE)
  	ylim <- range(data$AccY/data$VSum,na.rm=TRUE)
  	zlim <- range(data$AccZ/data$VSum,na.rm=TRUE)
    
    x = data[i, "AccX"]/data[i, "VSum"]
    y = data[i, "AccY"]/data[i, "VSum"]
    z = data[i, "AccZ"]/data[i, "VSum"]

    # x = c(data[seq(from=1,to=i,by=floor(nrow(data)/n)),"AccX"],data[seq(from=1,to=i,by=floor(nrow(data)/n)),"AccX"])
    # y = c(data[seq(from=1,to=i,by=floor(nrow(data)/n)),"AccY"],data[seq(from=1,to=i,by=floor(nrow(data)/n)),"AccY"])
    # z = c(data[seq(from=1,to=i,by=floor(nrow(data)/n)),"AccZ"],data[seq(from=1,to=i,by=floor(nrow(data)/n)),"AccZ"])
    
    require(scatterplot3d)
    scatterplot3d(x, y, z, xlim = xlim, ylim = ylim, zlim = zlim)
    # s <- scatterplot3d(x, y, z, xlim = xlim, ylim = ylim, zlim = zlim)
    # p1 <- s$xyz.convert(x[1],y[1],z[1])
    # p2 <- s$xyz.convert(x[2],y[2],z[2])
    # segments(p1$x,p1$y,p2$x,p2$y,lwd=2,col=2)

  }
}


dev.off()

require(rgl)
plot3d(data[, 1:3])

head(data)

plot(data$AccX/data$VSum, data$AccY/data$VSum, type = 'l')
lines(data$AccX_l/data$VSum, data$AccY_l/data$VSum, col = 'red')

data$AccX_l <- smooth.spline(data$AccX)$y
data$AccY_l <- smooth.spline(data$AccY)$y
data$AccZ_l <- smooth.spline(data$AccZ)$y

plot(data$VSum, type = 'l')

plot(data$AccX, data$AccY, type = 'l')
lines(data$AccX_l, data$AccY_l, col = 'red')


plot(data$AccX - data$AccX_l, data$AccY - data$AccY_l, type = 'l')
lines(data$AccX_l - data$AccX_l, data$AccY_l - data$AccY_l, col = 'red')

plot(data$AccX - data$AccX_l, type = 'l')

plot(cumsum(data$AccX - data$AccX_l), type = 'l')

plot(cumsum(cumsum(data$AccX - data$AccX_l)), type = 'l')

head(data)

plot(data$VSum)

dev.off()

require(animation)
n = 100
ani.options(interval=0.01/n)
# saveGIF(accel2d(n=50),movie.name=paste("2D_animation_",Trap,".gif",sep=""))
saveGIF(accel3d(n=100),movie.name=paste("3D_animation_.gif",sep=""))


AllCameraData$VSum <- sqrt(AllCameraData$AccX^2 + AllCameraData$AccY^2 + AllCameraData$AccZ^2)/1000

Extract_signal <- function(Set, Trap, Camera, EventNumber) {

	AllCameraData_stce <- AllCameraData[AllCameraData$Set == Set & 
	AllCameraData$Trap == Trap & 
	AllCameraData$Camera == Camera & 
	AllCameraData$EventNumber == EventNumber, ]

	AllCameraData_stce <- AllCameraData_stce[order(AllCameraData_stce$DateTime), ]

	return(cbind(AllCameraData_stce$AccX, AllCameraData_stce$AccY, AllCameraData_stce$AccZ))

}

Set = 36
Trap = 14
Camera = 2
EventNumber = 1

STRS2013_S0004T0014_GOPR0606
STRS2013_S0004T0014_GOPR0610

# Accelerometer_features(Set, Trap, Camera, EventNumber)

Accelerometer_features <- function(Set, Trap, Camera, EventNumber, V) {

	f = 5

	AccX <- Extract_signal(Set, Trap, Camera, EventNumber)[, 1]
	AccY <- Extract_signal(Set, Trap, Camera, EventNumber)[, 2]
	AccZ <- Extract_signal(Set, Trap, Camera, EventNumber)[, 3]

	VSum <- sqrt(AccX^2 + AccY^2 + AccZ^2)
	plot(VSum/1000, type = 'l')

	VSum_l = smooth.spline(VSum)$y

	velV = cumsum(VSum_l - median(VSum_l))*(1/f)
	# velV = cumsum(VSum*9.8/1000 - median(VSum*9.8/1000))*(1/f)
	
	PosV = cumsum(velV)*(1/f)

	ran = max(VSum*9.8/1000) - min(VSum*9.8/1000) + 1
	ran

	plot(VSum, type = 'l', ylim = c(9, 13))
	smth <- smooth.spline(VSum)
	lines(predict(smth)$y, col = 'red')
	dev.new()

	plot(VSum)
	lines(VSum_l)

	plot(VSum - median(VSum))
	lines(VSum_l - median(VSum_l))

	plot(VSum*9.8/1000 - median(VSum*9.8/1000), type = 'l', ylim = c(-1, 1))
	smth <- smooth.spline(VSum*9.8/1000 - median(VSum*9.8/1000))
	lines(predict(smth)$y, col = 'red')
	abline(h = 0)
	dev.new()

	plot(velV, type = 'l')
	dev.new()

	plot(PosV, type = 'l')
	dev.new()

	graphics.off()

	# min 
	minX = min(AccX)
	minY = min(AccY)
	minZ = min(AccZ)
	minV = min(VSum)

	# max
	maxX = max(AccX)
	maxY = max(AccY)
	maxZ = max(AccZ)
	maxV = max(VSum)

	# mean
	meanX = mean(AccX)
	meanY = mean(AccY)
	meanZ = mean(AccZ)
	meanV = mean(VSum)

	# sd
	sdX = sd(AccX)
	sdY = sd(AccY)
	sdZ = sd(AccZ)
	sdV = sd(VSum)

	# corXY
	corXY = cor(AccX, AccY)
	corXZ = cor(AccX, AccZ)
	corYZ = cor(AccY, AccZ)

	# median crossing rate
	require(seewave)	
	zcrX = zcr(AccX - median(AccX), f = f, wl = NULL)
	zcrY = zcr(AccY - median(AccY), f = f, wl = NULL)
	zcrZ = zcr(AccZ - median(AccZ), f = f, wl = NULL)
	zcrV = zcr(VSum - median(VSum), f = f, wl = NULL)

	# quantiles
	quanX = as.vector(quantile(AccX, prob = c(0.1, 0.25, 0.5, 0.75, 0.90)))
	quanY = as.vector(quantile(AccY, prob = c(0.1, 0.25, 0.5, 0.75, 0.90)))
	quanZ = as.vector(quantile(AccZ, prob = c(0.1, 0.25, 0.5, 0.75, 0.90)))
	quanV = as.vector(quantile(VSum, prob = c(0.1, 0.25, 0.5, 0.75, 0.90)))

	specX <- spec(AccX, f = f, plot = FALSE)
	specY <- spec(AccY, f = f, plot = FALSE)
	specZ <- spec(AccZ, f = f, plot = FALSE)
	specV <- spec(VSum, f = f, plot = FALSE)

	# spectral centriod
	centX = specprop(specX)$cent
	centY = specprop(specY)$cent
	centZ = specprop(specZ)$cent
	centV = specprop(specV)$cent

	# roughness
	roughnessX = roughness(AccX)
	roughnessY = roughness(AccY)
	roughnessZ = roughness(AccZ)
	roughnessV = roughness(VSum)

	# rms
	rmsX = rms(AccX)
	rmsY = rms(AccY)
	rmsZ = rms(AccZ)
	rmsV = rms(VSum)

	# q25
	Q25X = specprop(specX)$Q25
	Q25Y = specprop(specY)$Q25
	Q25Z = specprop(specZ)$Q25
	Q25V = specprop(specV)$Q25

	# q75
	Q75X = specprop(specX)$Q75
	Q75Y = specprop(specY)$Q75
	Q75Z = specprop(specZ)$Q75
	Q75V = specprop(specV)$Q75

	# Iqr
	IQRX = specprop(specX)$IQR
	IQRY = specprop(specY)$IQR
	IQRZ = specprop(specZ)$IQR
	IQRV = specprop(specV)$IQR

	# rugo
	rugoX = rugo(AccX)
	rugoY = rugo(AccY)
	rugoZ = rugo(AccZ)
	rugoV = rugo(VSum)

	# entropy
	require(tuneR)
	HX = H(Wave(AccX, samp.rate = f), wl = 4)
	HY = H(Wave(AccY, samp.rate = f), wl = 4)
	HZ = H(Wave(AccZ, samp.rate = f), wl = 4)
	HV = H(Wave(VSum, samp.rate = f), wl = 4)

	pgramX <- periodogram(Wave(AccX, samp.rate = f))
	pgramY <- periodogram(Wave(AccY, samp.rate = f))
	pgramZ <- periodogram(Wave(AccZ, samp.rate = f))
	pgramV <- periodogram(Wave(VSum, samp.rate = f))

	# variance
	varianceX = pgramX@variance
	varianceY = pgramY@variance
	varianceZ = pgramZ@variance
	varianceV = pgramV@variance

	# energy
	energyX = pgramX@energy
	energyY = pgramY@energy
	energyZ = pgramZ@energy
	energyV = pgramV@energy

	if(V == FALSE) {

		Accelerometer_features <- 
		c(
			minX, 
			minY, 
			minZ, 
			minV, 
			maxX, 
			maxY, 
			maxZ, 
			maxV, 
			meanX, 
			meanY, 
			meanZ, 
			meanV, 
			sdX, 
			sdY, 
			sdZ, 
			sdV, 
			corXY, 
			corXZ, 
			corYZ, 
			zcrX, 
			zcrY, 
			zcrZ, 
			zcrV, 
			quanX, 
			quanY, 
			quanZ, 
			quanV, 
			centX, 
			centY, 
			centZ, 
			centV, 
			roughnessX, 
			roughnessY, 
			roughnessZ, 
			roughnessV, 
			rmsX, 
			rmsY, 
			rmsZ, 
			rmsV, 
			Q25X, 
			Q25Y, 
			Q25Z, 
			Q25V, 
			Q75X, 
			Q75Y, 
			Q75Z, 
			Q75V, 
			IQRX, 
			IQRY, 
			IQRZ, 
			IQRV, 
			rugoX, 
			rugoY, 
			rugoZ, 
			rugoV, 
			HX, 
			HY, 
			HZ, 
			HV, 
			varianceX, 
			varianceY, 
			varianceZ, 
			varianceV, 
			energyX, 
			energyY, 
			energyZ, 
			energyV
			)

	} else {

		Accelerometer_features <- 
		c(
			# minX, 
			# minY, 
			# minZ, 
			minV, 
			# maxX, 
			# maxY, 
			# maxZ, 
			maxV, 
			# meanX, 
			# meanY, 
			# meanZ, 
			meanV, 
			# sdX, 
			# sdY, 
			# sdZ, 
			sdV, 
			# corXY, 
			# corXZ, 
			# corYZ, 
			# zcrX, 
			# zcrY, 
			# zcrZ, 
			zcrV, 
			# quanX, 
			# quanY, 
			# quanZ, 
			quanV, 
			# centX, 
			# centY, 
			# centZ, 
			centV, 
			# roughnessX, 
			# roughnessY, 
			# roughnessZ, 
			roughnessV, 
			# rmsX, 
			# rmsY, 
			# rmsZ, 
			rmsV, 
			# Q25X, 
			# Q25Y, 
			# Q25Z, 
			Q25V, 
			# Q75X, 
			# Q75Y, 
			# Q75Z, 
			Q75V, 
			# IQRX, 
			# IQRY, 
			# IQRZ, 
			IQRV, 
			# rugoX, 
			# rugoY, 
			# rugoZ, 
			rugoV, 
			# HX, 
			# HY, 
			# HZ, 
			HV, 
			# varianceX, 
			# varianceY, 
			# varianceZ, 
			varianceV, 
			# energyX, 
			# energyY, 
			# energyZ, 
			energyV
			)

	}


Accelerometer_feature_names <- 
c("minX", 
	"minY", 
	"minZ", 
	"minV", 
	"maxX", 
	"maxY", 
	"maxZ", 
	"maxV", 
	"meanX", 
	"meanY", 
	"meanZ", 
	"meanV", 
	"sdX", 
	"sdY", 
	"sdZ", 
	"sdV", 
	"corXY", 
	"corXZ", 
	"corYZ", 
	"zcrX", 
	"zcrY", 
	"zcrZ", 
	"zcrV", 
	"quanX", 
	"quanY", 
	"quanZ", 
	"quanV", 
	"centX", 
	"centY", 
	"centZ", 
	"centV", 
	"roughnessX", 
	"roughnessY", 
	"roughnessZ", 
	"roughnessV", 
	"rmsX", 
	"rmsY", 
	"rmsZ", 
	"rmsV", 
	"Q25X", 
	"Q25Y", 
	"Q25Z", 
	"Q25V", 
	"Q75X", 
	"Q75Y", 
	"Q75Z", 
	"Q75V", 
	"IQRX", 
	"IQRY", 
	"IQRZ", 
	"IQRV", 
	"rugoX", 
	"rugoY", 
	"rugoZ", 
	"rugoV", 
	"HX", 
	"HY", 
	"HZ", 
	"HV", 
	"varianceX", 
	"varianceY", 
	"varianceZ", 
	"varianceV", 
	"energyX", 
	"energyY", 
	"energyZ", 
	"energyV")

	return(as.vector(Accelerometer_features))
}

collapse <- function(Outputs, Outputs_f, result) {

	result_collapse <- matrix(NA, ncol = length(Outputs), nrow = nrow(result))

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




