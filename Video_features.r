setwd(master_dir)

to_dir = paste(clust_dir, "/cur/Clear/Features", sep = "")
frame_dir = frame_dir
require(stringr)
if(length(list.files(paste(clust_dir, "/cur/Clear", sep = ""), pattern = "\\.jpg")) > 0) {
  video_files = str_match(list.files(paste(clust_dir, "/cur/Clear", sep = ""), pattern = "\\.jpg"), "(.+)\\.jpg")[, 2]
} else {
  video_files <- vector()
}

if(file.exists(paste("data_areas", year, ".RData", sep = "")) == TRUE) {
  load(file = paste("data_areas", year, ".RData", sep = ""))
  data_areas_cur = data_areas
  video_files_cur = data_areas_cur$Filenames
  video_files_cur
} else {
  data_areas_cur = data.frame()
  video_files_cur <- vector()
}

video_files_new = video_files[!(video_files %in% video_files_cur)]
video_files_new

# # deletes rep_frames folder, then creates it (aviods overlap)
# system(paste('rm -r ', to_dir, sep = ""))
# system(paste('mkdir ', to_dir, sep = ""))

video_file = 'STRS2013_S035T015C000_GOPR0280'
video_file = video_files_cur[1]

if(length(video_files_new) > 0) {

  Detect_features <- function(video_file, frame_dir, to_dir, W, H, skip) {

    print(paste("Detecting features: ", video_file, sep = ""))

    require(rPython)
    setwd(master_dir)
    python.assign("video_file", video_file)
    python.assign("frame_dir", frame_dir)
    python.assign("to_dir", to_dir)
    python.assign("skip", skip)
    python.assign("W", W)
    python.assign("H", H)
    python.assign("data", 0)
    python.load("Mean_frame.py")
    python.load("Reduce_colors.py")
    python.load("Remove_trap.py")
    python.load("Resize_frame.py")
    python.load("Detect_features.py")
    python.exec("
    os.chdir(frame_dir)
    frame = cv2.imread(video_file + '.jpg')
    frame = Remove_trap(frame)
    data_areas = Detect_features( frame, to_dir, video_file + '.jpg' )
  ")
    # frame = Resize_frame(frame, W, H)

    data_areas_new = python.get("data_areas")

    if(length(data_areas_new) == 0) {
      return(rep(0, 25))
    }

    if(length(data_areas_new) >= 3) {

      km <- kmeans(x = data_areas_new, centers = 2)
      n_centers = 2

      } else if(length(data_areas_new) >= 1) {

      km <- kmeans(x = data_areas_new, centers = 1)
      n_centers = 1

    }

    cent1 <- min(km$centers)
    cent2 <- max(km$centers)
    cluster <- km$cluster

    if(km$centers[1] != cent1) {
      cluster <- rep(3, length(cluster)) - cluster
    }

    clust1 <- min(cluster)
    clust2 <- max(cluster)

    data_areas_new1 <- data_areas_new[cluster == clust1]
    data_areas_new2 <- data_areas_new[cluster == clust2] 
    data_areas_new0 <- data_areas_new

    data_areas_new <- c(cent2 - cent1, 
      c(summary(data_areas_new1), length(data_areas_new1[data_areas_new1 != 0]), ifelse(is.na(sd(data_areas_new1)), 0, sd(data_areas_new1))), 
      c(summary(data_areas_new2), length(data_areas_new2[data_areas_new2 != 0]), ifelse(is.na(sd(data_areas_new2)), 0, sd(data_areas_new2))),
      c(summary(data_areas_new0), length(data_areas_new0[data_areas_new0 != 0]), ifelse(is.na(sd(data_areas_new0)), 0, sd(data_areas_new1)))
      )
    names(data_areas_new) <- paste("Areas", seq(1, length(data_areas_new), 1), sep = "")
    print(data_areas_new)

    return(data_areas_new)

  }

  if(mac == TRUE) {
    data_areas_new = lapply(video_files, FUN = function(video_file) Detect_features(video_file
      , frame_dir, to_dir, W, H, skip))
  } else {
    data_areas_new = mclapply(video_files, FUN = function(video_file) Detect_features(video_file
      , frame_dir, to_dir, W, H, skip)
      , mc.cores = n_cores
      , mc.silent = FALSE
      , mc.preschedule = TRUE
      )

  }

  data_areas_new <- matrix(unlist(data_areas_new), nrow = length(data_areas_new), byrow = TRUE)
  data_areas_new <- as.data.frame(data_areas_new)
  names(data_areas_new) <- paste("Areas", seq(1, ncol(data_areas_new), 1), sep = "")
  row.names(data_areas_new) <- video_files
  data_areas_new$Filenames <- video_files

  require(plyr)
  data_areas = rbind.fill(data_areas_cur, data_areas_new)

  setwd(master_dir)
  save(data_areas, file = paste("data_areas", year, ".RData", sep = ""))

}

head(data_areas)


# ----------------------------------------------------

# video_files <- video_files_cur[ImageClarity == "Clear"]
# video_files

# to_dir <- master_dir
# to_dir <- paste(frame_dir, "/", "Clear", "/", "features", sep = "")

# video_file <- "STRS2013_S0036T0014_GOPR0347.mp4"
# # video_file <- "STRS2013_S0068T0014_GOPR0302.mp4"

# require(rPython)
# setwd(master_dir)
# python.load("Features.py")
# python.call("Features", video_file, W, H, video_dir, skip, to_dir )




