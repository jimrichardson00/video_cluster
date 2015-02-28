# ------------------------------------------------------
# extract data and perform PCA

setwd(audio_dir)

audio_files <- sort(list.files(audio_dir, pattern = "\\.wav$"))
audio_files

# pulls out spectrogram at 10 second mark
require(parallel)
require(tuneR)
audio_files_L <- mclapply(X = audio_files, FUN = function(audio_file) Audio_features(audio_file)
	, mc.cores = n_cores
	, mc.silent = FALSE
	, mc.preschedule = FALSE
	)

audio_files_L[[1]]

# audio data
audio_files_M <- matrix(unlist(audio_files_L), nrow = length(audio_files), byrow = TRUE)
audio_files_M

require(stats)
prcomp <- prcomp(audio_files_M, scale. = TRUE)

components_ <- prcomp$rotation
prx <- prcomp$x

# update ccipca list with new values
pca <- list()
pca[["components_"]] <- components_  
pca[["audio_files_M"]] <- audio_files_M  
pca[["audio_files"]] <- audio_files  
pca[["prx"]] <- prx

# write pca list to RData
setwd(master_dir)
save(pca, file = paste('pca_audio', year, '.RData', sep = ''))
