# ------------------------------------------------------
# load pca data and subset

load(paste("ccipca_video", year, ".RData", sep = ""))

prx <- ccipca[["prx"]]
video_files_cur <- ccipca[["video_files_cur"]]
prx <- as.data.frame(prx)
names(prx) <- paste("Video", seq(1, ncol(prx), 1), sep = "")
# convert to numeric
for(name in names(prx)){
	prx[, name] <- as.numeric(prx[, name])
}
# # scale so that variance  = 1
# for(j in 1:ncol(prx)) {
# 	if(is.numeric(prx[, j]) == TRUE) {
# 		prx[, j] <- prx[, j]/sd(prx[, j])
# 	}
# }
require(stringr)
prx$Filenames <- ccipca[["video_files_cur"]]
clusters <- ccipca[["clusters"]]
prx$ImageClarity <- ImageClarity
prx$IfBiotic <- IfBiotic

Clear <- ccipca[["Clear"]]
Cloudy <- ccipca[["Cloudy"]]

prx <- prx[prx$Filenames %in% Clear | prx$Filenames %in% Cloudy, ]

# ------------------------------------------------------
# load video log data
setwd(master_dir)
if(mac == FALSE) {

	require(Hmisc)
	Sablefish_Survey_2013 <- mdb.get("2013_Sablefish_Survey.mdb")
	Video_Analysis_Log <- Sablefish_Survey_2013$data

	require(stringr)
	Video_Analysis_Log$Set <- as.integer(str_match(Video_Analysis_Log$TransectName, "set(.+)camera(.+)")[, 2])
	Video_Analysis_Log$Set <- formatC(Video_Analysis_Log$Set, width = 3, flag = "0")
	Video_Analysis_Log$Trap <- as.integer(str_match(Video_Analysis_Log$TransectName, "trap(.+)")[, 2])
	Video_Analysis_Log$Trap <- formatC(Video_Analysis_Log$Trap, width = 3, flag = "0")
	Video_Analysis_Log$Filenames <- paste("STRS2013", "_", "S", Video_Analysis_Log$Set, "T", Video_Analysis_Log$Trap, "_", str_match(Video_Analysis_Log$FileName, "(.+)\\.MP4")[, 2], sep = "")

	require(plyr)
	d <- ddply(.data = Video_Analysis_Log, .variables = .(TransectName, FileName), .fun = summarise, maxID = max(ID))
	d$Set <- as.integer(str_match(d$TransectName, "set(.+)camera(.+)")[, 2])
	d$Set <- formatC(d$Set, width = 3, flag = "0")
	d$Trap <- as.integer(str_match(d$TransectName, "trap(.+)")[, 2])
	d$Trap <- formatC(d$Trap, width = 3, flag = "0")
	d$Filenames <- paste("STRS2013", "_", "S", d$Set, "T", d$Trap, "_", str_match(d$FileName, "(.+)\\.MP4")[, 2], sep = "")

	Video_Analysis_Log_m <- merge(x = Video_Analysis_Log, y = d, by.x = "Filenames", by.y = "Filenames", all = FALSE)
	Video_Analysis_Log_mu <- Video_Analysis_Log_m[Video_Analysis_Log_m$ID == Video_Analysis_Log_m$maxID, ]

	Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE | Video_Analysis_Log_mu == ""] <- "NA"
	for(name in names(Video_Analysis_Log_mu)){
		Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
	}

	Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = Sablefish_Survey_2013$lu_sounds, by.x = "Sounds", by.y = "SoundsID", all.x = TRUE)
	Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = Sablefish_Survey_2013$lu_substrate, by.x = "DominantSubstrate", by.y = "SubstrateId", all.x = TRUE)

	Video_Analysis_Log_mu$SoundName <- gsub("/", "_", Video_Analysis_Log_mu$SoundName)

	Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE | Video_Analysis_Log_mu == ""] <- "NA"
	for(name in names(Video_Analysis_Log_mu)){
		Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
	}

	head(Video_Analysis_Log_mu)

	write.csv(Video_Analysis_Log_mu, "Video_Analysis_Log_mu.csv")

} else {

	Video_Analysis_Log_mu <- read.csv("Video_Analysis_Log_mu.csv")

	Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE | Video_Analysis_Log_mu == ""] <- "NA"
	for(name in names(Video_Analysis_Log_mu)){
		Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
	}

}

# ------------------------------------------------------
# merged data set
data_o <- merge(x = prx, y = Video_Analysis_Log_mu, by.x = "Filenames", by.y = "Filenames", all.x = TRUE)

setwd(master_dir)
if(file.exists(paste("data_areas", year, ".RData", sep = "")) == TRUE & c("IfBiotic") %in% Outputs) {
	load(paste("data_areas", year, ".RData", sep = ""))
	data <- merge(x = data_o, y = areas, by.x = "Filenames", by.y = "Filenames", all = TRUE)
} else {
	data <- data_o	
}

save(data, file = paste("data_video", year, ".RData", sep = ""))
write.csv(data, file = paste("data_video", year, ".csv", sep = ""))
