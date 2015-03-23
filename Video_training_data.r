# -------------------------------------
# Output:
# - Loads ccipca data
# - Creates new data set containing video_file name, Princ1, Princ2,.. and the following columns:
# ImageClarity:
#   If .jpg not in (clust_dir)/cur/ImageClarity_validated mark as NA
#   For remaining .jpg, if not in (clust_dir)/cur/Clear mark as Cloudy
#   For remaining .jpg, mark as Clear
# BioticMaterial:
#   If .jpg not in (clust_dir)/cur/Clear/BioticMaterial_validated mark as NA
#   For remaining .jpg, if not in (clust_dir)/cur/Clear/Presence mark as Absence
#   For remaining .jpg, mark as Presence
# - If .RData file 'data_areas.RData' exists, joins it to current data set. This contains colour blob area information for each of the Clear images.
# - If .RData file 'data_analy.RData' exists, joins it to current data set. This contains species information and other analysis from manual video analysis for all of the images.
# - Saves data set to data_train.RData file, will use this to train classifier.
# -------------------------------------

# -------------------------------------
# - Code loads ccipca data

# load ccipca data
load(paste("ccipca_video", year, ".RData", sep = ""))

# -------------------------------------
# - Creates new data set containing video_file name, Princ1, Princ2,.. and the following columns:
# ImageClarity:
#   If .jpg not in (clust_dir)/cur/ImageClarity_validated mark as NA
#   For remaining .jpg, if not in (clust_dir)/cur/Clear mark as Cloudy
#   For remaining .jpg, mark as Clear
# BioticMaterial:
#   If .jpg not in (clust_dir)/cur/Clear/BioticMaterial_validated mark as NA
#   For remaining .jpg, if not in (clust_dir)/cur/Clear/Presence mark as Absence
#   For remaining .jpg, mark as Presence

# load reduced dimension data set
prx <- ccipca[["prx"]]
video_files_cur <- ccipca[["video_files_cur"]]
prx <- as.data.frame(prx)
names(prx) <- paste("Princ", seq(1, ncol(prx), 1), sep = "")

# convert prx to numeric
for(name in names(prx)){
  prx[, name] <- as.numeric(prx[, name])
}
# scale so that variance  = 1
for(j in 1:ncol(prx)) {
  if(is.numeric(prx[, j]) == TRUE) {
  prx[, j] <- prx[, j]/sd(prx[, j])
  }
}

# create Filenames column in prx
prx$Filenames <- ccipca[["video_files_cur"]]

# sets Clear as list of files in (clust_dir)/cur/Clear
Clear <- character(0)
tryCatch({
  require(stringr)
  Clear <- str_match(list.files(paste(clust_dir, "/cur/Clear", sep = ""), pattern = ".jpg"), "(.+)\\.jpg")[, 2]
  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )

# sets ImageClarity_validated as list of files in (clust_dir)/cur/ImageClarity_validated
ImageClarity_validated <- character(0)
tryCatch({
  require(stringr)
  ImageClarity_validated <- na.omit(str_match(list.files(paste(clust_dir, "/cur/ImageClarity_validated", sep = ""), pattern = ".jpg"), "(.+)\\.jpg")[, 2])
  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )

# sets Presence as list of files in (clust_dir)/cur/Clear/Presence
Presence <- character(0)
tryCatch({
  require(stringr)
  Presence <- str_match(list.files(paste(clust_dir, "/cur/Clear/Presence", sep = "")), "(.+)\\.jpg")[, 2]
  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )

# sets BioticMaterial_validated as list of files in (clust_dir)/cur/BioticMaterial_validated
BioticMaterial_validated <- character(0)
tryCatch({
  require(stringr)
  BioticMaterial_validated <- na.omit(str_match(list.files(paste(clust_dir, "/cur/Clear/BioticMaterial_validated", sep = ""), pattern = ".jpg"), "(.+)\\.jpg")[, 2])
  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )

# Set ImageClarity as:
#   If .jpg not in 'ImageClarity_validated' mark as NA
#   For remaining .jpg, if not in 'Clear' mark as Cloudy
#   For remaining .jpg, mark as Clear
# Set ImageClarity as column in prx
ImageClarity <- ifelse(prx$Filenames %in% Clear, "Clear", ifelse(prx$Filenames %in% ImageClarity_validated, "Cloudy", NA))
prx$ImageClarity <- ImageClarity
prx$ImageClarity

# Set BioticMaterial as:
#   If .jpg not in 'BioticMaterial_validated' mark as NA
#   For remaining .jpg, if not in 'Presence' mark as Absence
#   For remaining .jpg, mark as Presence
# Set BioticMaterial as column in prx
BioticMaterial <- ifelse(prx$Filenames %in% Presence, "Presence", ifelse(prx$Filenames %in% BioticMaterial_validated, "Absence", NA))
prx$BioticMaterial <- BioticMaterial
prx$BioticMaterial

# # comment this out
# str1 <- str_match(prx$Filenames, "(.+T[0-9][0-9][0-9])(.+)")[, 2]
# str2 <- str_match(prx$Filenames, "(.+T[0-9][0-9][0-9])(.+)")[, 3]
# prx$Filenames <- paste(str1, "C000", str2, sep = "")
# prx$Filenames

# store prx in data_trai1
data_trai1 <- prx
head(data_trai1)

# -------------------------------------
# - If .RData file 'data_areas.RData' exists, joins it to current data set. This contains colour blob area information for each of the Clear images.

setwd(master_dir)
file.exists(paste("data_areas", year, ".RData", sep = "")) == TRUE
if(file.exists(paste("data_areas", year, ".RData", sep = "")) == TRUE) {

  # loads data_areas.RData
  load(file = paste("data_areas", year, ".RData", sep = ""))

  # # comment this out
  # str1 <- str_match(data_areas$Filenames, "(.+T[0-9][0-9][0-9])(.+)")[, 2]
  # str2 <- str_match(data_areas$Filenames, "(.+T[0-9][0-9][0-9])(.+)")[, 3]
  # data_areas$Filenames <- paste(str1, "C000", str2, sep = "")
  # data_areas$Filenames

  # stores column names containing 'Areas' as names, this is area info
  require(stringr)
  names <- as.vector(na.omit(str_match(names(data_areas), "Areas.+")))

  # convert to numeric
  for(name in names){
  	data_areas[, name] <- as.numeric(data_areas[, name])
  }
  # scale so that variance  = 1
  for(j in 1:ncol(data_areas)) {
  if(is.numeric(data_areas[, j]) == TRUE) {
		data_areas[, j] <- data_areas[, j]/sd(data_areas[, j])
		}
	}

  # join to current data set, name new data set data_trai2
  data_trai2 <- merge(x = data_trai1, y = data_areas, by.x = "Filenames", by.y = "Filenames", all = TRUE)

} else {

  # set data_trai2 as data_trai1 if data_areas.RData dodes not exist
	data_trai2 <- data_trai1

}

head(data_trai2)

# -------------------------------------
# - If .RData file 'data_analy.RData' exists, joins it to current data set. This contains species information and other analysis from manual video analysis for all of the images.

setwd(master_dir)
file.exists(paste("data_analy", year, ".RData", sep = "")) == TRUE
if(file.exists(paste("data_analy", year, ".RData", sep = "")) == TRUE) {

  # loads data_analy.RData
  load(file = paste("data_analy", year, ".RData", sep = ""))

  # join to current data set, name new data set data_trai3
  data_trai3 <- merge(x = data_trai2, y = data_analy, by.x = "Filenames", by.y = "Filenames", all = TRUE)

} else {

  # set data_trai3 as data_trai2 if data_areas.RData dodes not exist
  data_trai3 <- data_trai2  

}

# ------------------------------------
# - Saves data set to data_train.RData file, will use this to train classifier.

setwd(master_dir)

data_train <- data_trai3
save(data_train, file = paste("data_train", year, ".RData", sep = ""))

head(data_train)

# ------------------------------------
