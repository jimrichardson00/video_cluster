# ------------------------------------------------------
# set outputs and inputs
Inputs <- paste("PC", seq(1, 23, 1), sep = "")

# ------------------------------------------------------
# load pca data and subset

setwd(master_dir)
load(paste("pca_audio", year, ".RData", sep = ""))

# prx <- pca[["prx"]]
# prx <- as.data.frame(prx)
# names(prx) <- paste("PC", seq(1, ncol(prx), 1), sep = "")
# for(name in names(prx)){
# 	prx[, name] <- as.numeric(prx[, name])
# }
# audio_files <- pca[["audio_files"]]
# require(stringr)
# prx$filenames <- str_match(audio_files, "(.+)\\.wav")[, 2]

prx <- pca[["audio_files_M"]]
prx <- as.data.frame(prx)

# convert to numeric
for(name in names(prx)){
	prx[, name] <- as.numeric(prx[, name])
}

# scale so that variance  = 1
for(j in 1:ncol(prx)) {
	if(is.numeric(prx[, j]) == TRUE) {
		prx[, j] <- prx[, j]/sd(prx[, j])
	}
}

names(prx) <- paste("PC", seq(1, ncol(prx), 1), sep = "")
audio_files <- pca[["audio_files"]]
require(stringr)
prx$Filenames <- str_match(audio_files, "(.+)\\.wav")[, 2]

# ------------------------------------------------------
# load video log data
if(mac == FALSE) {

	require(Hmisc)
	Sablefish_Survey_2013 <- mdb.get("2013_Sablefish_Survey.mdb")
	Video_Analysis_Log <- Sablefish_Survey_2013$data

	require(stringr)
	Video_Analysis_Log$Set <- as.integer(str_match(Video_Analysis_Log$TransectName, "set(.+)camera(.+)")[, 2])
	Video_Analysis_Log$Set <- formatC(Video_Analysis_Log$Set, width = 4, flag = "0")
	Video_Analysis_Log$Trap <- as.integer(str_match(Video_Analysis_Log$TransectName, "trap(.+)")[, 2])
	Video_Analysis_Log$Trap <- formatC(Video_Analysis_Log$Trap, width = 4, flag = "0")
	Video_Analysis_Log$Filenames <- paste("STRS2013", "_", "S", Video_Analysis_Log$Set, "T", Video_Analysis_Log$Trap, "_", str_match(Video_Analysis_Log$FileName, "(.+)\\.MP4")[, 2], sep = "")

	require(plyr)
	d <- ddply(.data = Video_Analysis_Log, .variables = .(TransectName, FileName), .fun = summarise, maxID = max(ID))
	d$Set <- as.integer(str_match(d$TransectName, "set(.+)camera(.+)")[, 2])
	d$Set <- formatC(d$Set, width = 4, flag = "0")
	d$Trap <- as.integer(str_match(d$TransectName, "trap(.+)")[, 2])
	d$Trap <- formatC(d$Trap, width = 4, flag = "0")
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

data <- merge(x = prx, y = Video_Analysis_Log_mu, by.x = "Filenames", by.y = "Filenames", all = FALSE)
data[is.na(data) == TRUE | data == ""] <- "NA"

head(prx)
head(Video_Analysis_Log_mu)

save(data, file = "data_audio.RData")

if(Sounds == TRUE) {
	data <- data[data$Sounds != "0", ]
}

# unique set, trap combos
set_camera <- str_match(data$Filenames, "(.+)_GOPR.+")[, 2] 
data$set_camera <- set_camera
set_camera_u <- unique(set_camera)

# ------------------------------------------------------
# set new output list with 0, 1 true false for each category within each variable

Outputs_f <- vector()
for(Output in Outputs) {
	Outputs_f_o <- unique(data[, Output])
	for(Output_f_o in Outputs_f_o) {
		data[, paste(Output, Output_f_o, sep = "")] <- as.numeric(ifelse(data[, Output] == Output_f_o, 1, 0))
		print(paste(Output, " ", Output_f_o, sep = ""))
		Outputs_f <- c(Outputs_f, paste(Output, Output_f_o, sep = ""))
	}
	# data[, Output] <- factor(data[, Output])
}

# formula for factors, and for regular
formula_f <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs, collapse = "+")))
formula <- as.formula(paste(paste(paste("factor(", Outputs, ")", sep = ""), collapse = "+"), "~", paste(Inputs, collapse = "+")))

# ------------------------------------------------------

results <- vector("list", N)
n_nodes <- ceiling(mean(c(length(Outputs_f), length(Inputs))))
n_layer <- 1
require(parallel)
i <- 1
# for(i in seq(1, N, 1)) {
results <- foreach(i = seq(1, N, 1)) %dopar% {

	print(i)

	SC_sc <- sample(seq(1, length(set_camera_u), 1), size = ceiling(length(set_camera_u)*(2/3)))
	train <- seq(1, nrow(data), 1)[set_camera %in% set_camera_u[SC_sc]]
  
	train <- sample(seq(1, nrow(data), 1), size = ceiling(nrow(data)*(2/3)))

	data_train <- data[train, ]
	data_test <- data[-train, ]

	tryCatch({

		# artificial neural network
		require(neuralnet)
		arnn <- neuralnet(formula_f, rep = 1, stepmax = 10^6, data = data_train)
		arnn_result <- compute(x = arnn, data_test[, Inputs])

		# random forest and naive bays
		rndf_result <- matrix(NA, nrow = nrow(data_test), ncol = length(Outputs))
		nbay_result <- matrix(NA, nrow = nrow(data_test), ncol = length(Outputs))
		o <- 1
		for(o in seq(1, length(Outputs), 1)) {

			Output <- Outputs[o]

			# naive bayes
			require(klaR)
			nbay <- NaiveBayes(formula = formula, data = data_train, prior = rep(1/length(unique(data_train[, Outputs])), length(unique(data_train[, Outputs]))))
			nbay_result[, o] <- paste(Output, as.vector(predict(object = nbay, newdata = data_test[, Inputs])$class), sep = "")

			# random forest
			require(randomForest)
			rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
				, data = data_train
				, replace = TRUE
				, strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
				)
			rndf_result[, o] <- paste(Output, as.vector(predict(object = rndf, newdata = data_test)), sep = "")
				
		}
		nbay_result <- as.data.frame(nbay_result)
		names(nbay_result) <- Outputs
		rndf_result <- as.data.frame(rndf_result)
		names(rndf_result) <- Outputs

		# crude mode classifer
		data_train_c <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_train[, Outputs_f])
		mode_table <- apply(data_train_c, MARGIN = 2, FUN = function(x) names(which.max(table(x))))
		mode_result <- matrix(rep(mode_table, nrow(data_test)), nrow = nrow(data_test), byrow = TRUE)
		mode_result <- as.data.frame(mode_result)
		names(mode_result) <- names(mode_table)

		# confusion matrix
		results[[i]][["arnn_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = arnn_result$net.result)
		results[[i]][["rndf_result"]] <- rndf_result
		results[[i]][["nbay_result"]] <- nbay_result
		results[[i]][["mode_result"]] <- mode_result
		results[[i]][["data_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_test[, Outputs_f])

		results[[i]]

	},
	error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
	)
}

results

non_null <- seq(1, N, 1)[unlist(lapply(1:N, FUN = function(i) is.null(results[[i]]) == FALSE))]
non_null

results_nn <- vector("list", length(non_null))
for(i in 1:length(non_null)) {
	results_nn[[i]] <- results[[non_null[i]]]
}

per_correct_resu <- list()
for(o in seq(1, length(Outputs), 1)) {
	Output <- Outputs[o]

	Outputs_f_o <- na.omit(str_match(Outputs_f, paste(Output, ".+", sep = "")))
	Outputs_f_o

	confusion_arnn <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
	confusion_rndf <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
	confusion_nbay <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
	confusion_mode <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))

	per_correct_arnn <- 0
	per_correct_rndf <- 0
	per_correct_nbay <- 0
	per_correct_mode <- 0

	per_correct_arnns <- vector()
	per_correct_rndfs <- vector()
	per_correct_nbays <- vector()
	per_correct_modes <- vector()

	for(i in 1:length(results_nn)){

		confusion_arnn <- confusion_arnn + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")
		confusion_rndf <- confusion_rndf + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")
		confusion_nbay <- confusion_nbay + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")
		confusion_mode <- confusion_mode + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")

		# per_correct_arnn <- per_correct_arnn + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "arnn_result"))
		# per_correct_rndf <- per_correct_rndf + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "rndf_result"))
		# per_correct_nbay <- per_correct_nbay + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "nbay_result"))
		# per_correct_mode <- per_correct_mode + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "mode_result"))

		per_correct_arnns <- c(per_correct_arnns, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")))
		per_correct_rndfs <- c(per_correct_rndfs, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")))
		per_correct_nbays <- c(per_correct_nbays, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")))
		per_correct_modes <- c(per_correct_modes, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")))

	}

	per_correct_resu <- list(per_correct_arnns = per_correct_arnns,
		per_correct_rndfs = per_correct_rndfs,
		per_correct_nbays = per_correct_nbays,
		per_correct_modes = per_correct_modes)

	print(Outputs[o])
	print(paste("arnn", " ", mean(per_correct_arnns), sep = ""))
	print(paste("rndf", " ", mean(per_correct_rndfs), sep = ""))
	print(paste("nbay", " ", mean(per_correct_nbays), sep = ""))
	print(paste("mode", " ", mean(per_correct_modes), sep = ""))
	print("")

}

save(per_correct_resu, file = paste("per_correct_audio", paste(Outputs, collapse = ""), ".RData", sep = ""))

