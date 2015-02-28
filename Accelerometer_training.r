setwd(master_dir)

AllCameraData <- read.csv("AllCameraData.CSV")
AllCameraData <- AllCameraData[AllCameraData$Set != 45 | 
AllCameraData$Camera != 5 |
AllCameraData$Trap != 14 |
AllCameraData$EventNumber != 2, ]
AllCameraData <- AllCameraData[AllCameraData$Set != 29 | 
AllCameraData$Camera != 5 |
AllCameraData$Trap != 1, ]
AllCameraData <- AllCameraData[AllCameraData$Set != 30 | 
AllCameraData$Camera != 8 |
AllCameraData$Trap != 14, ]
AllCameraData <- AllCameraData[AllCameraData$Set != 42 | 
AllCameraData$Trap != 14, ]
AllCameraData <- AllCameraData[AllCameraData$Set != 84 | 
AllCameraData$Trap != 14, ]
AllCameraData <- AllCameraData[AllCameraData$Set != 91 | 
AllCameraData$Trap != 25, ]

AllCameraData$VSum <- sqrt(AllCameraData$AccX^2 + AllCameraData$AccY^2 + AllCameraData$AccZ^2)/1000

# STCE <- unique(AllCameraData[, c("Set", "Trap", "Camera", "EventNumber")])
# head(STCE)

# AF <-t(mapply(FUN = Accelerometer_features, Set = STCE$Set, Trap = STCE$Trap, Camera = STCE$Camera, EventNumber = STCE$EventNumber, V = V))
# AF <- as.data.frame(AF)
# head(AF)

# STCE_AF <- cbind(STCE, AF)
# head(STCE_AF)

# write.csv(STCE_AF, file = "STCE_AF.csv")

# ----------------------------

STCE_AF <- read.csv("STCE_AF.csv")

# ----------------------------

Clips_vs_Records <- read.csv("Clips vs Records.csv")
Clips_vs_Records

# ---------------------------

require(Hmisc)

names(Sablefish_Survey_2013)

Sablefish_Survey_2013 <- mdb.get("2013_Sablefish_Survey.mdb")
Video_Analysis_Log <- Sablefish_Survey_2013$data
write.csv(Video_Analysis_Log, file = "Video_Analysis_Log.csv")
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
Video_Analysis_Log_mu[is.na(Video_Analysis_Log_mu) == TRUE] <- "NA"
for(name in names(Video_Analysis_Log_mu)){
	Video_Analysis_Log_mu[, name] <- as.character(Video_Analysis_Log_mu[, name])
}
Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = Sablefish_Survey_2013$lu_sounds, by.x = "Sounds", by.y = "SoundsID", all.x = TRUE)
Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = Sablefish_Survey_2013$lu_substrate, by.x = "DominantSubstrate", by.y = "SubstrateId", all.x = TRUE)
Video_Analysis_Log_mu  <- merge(x = Video_Analysis_Log_mu, y = Sablefish_Survey_2013$lu_trap_motion, by.x = "TrapMotion", by.y = "TrapMotionID", all.x = TRUE)
Video_Analysis_Log_mu$SoundName <- gsub("/", "_", Video_Analysis_Log_mu$SoundName)
Video_Analysis_Log_mu$Filenames <- paste("STRS2013", "_", "S", Video_Analysis_Log_mu$Set.x, "T", Video_Analysis_Log_mu$Trap.x, "_", str_match(Video_Analysis_Log_mu$FileName.x, "(.+)\\.MP4")[, 2], sep = "")
Video_Analysis_Log_mu <- Video_Analysis_Log_mu[paste(Video_Analysis_Log_mu$Filenames, ".mp4", sep = "") %in% list.files(video_dir), ]
head(Video_Analysis_Log_mu)

# ------------------------

Clips_vs_Records$Set <- formatC(Clips_vs_Records$Set, width = 4, flag = "0")
Clips_vs_Records$Trap <- formatC(Clips_vs_Records$Trap, width = 4, flag = "0")
Clips_vs_Records$Set_Trap <- paste(Clips_vs_Records$Set, "_", Clips_vs_Records$Trap, sep = "")
Clips_vs_Records <- Clips_vs_Records[Clips_vs_Records$Events.Clips.match. == "yes" & Clips_vs_Records$Note == "", ]
Clips_vs_Records

STCE_AF$Set <- formatC(STCE_AF$Set, width = 4, flag = "0")
STCE_AF$Trap <- formatC(STCE_AF$Trap, width = 4, flag = "0")
STCE_AF$Set_Trap <- paste(STCE_AF$Set, "_", STCE_AF$Trap, sep = "")
STCE_AF <- STCE_AF[order(STCE_AF$Set_Trap, STCE_AF$EventNumber), ]

Video_Analysis_Log_mu$Set <- formatC(as.integer(Video_Analysis_Log_mu$Set.x), width = 4, flag = "0")
Video_Analysis_Log_mu$Trap <- formatC(as.integer(Video_Analysis_Log_mu$Trap.x), width = 4, flag = "0")
Video_Analysis_Log_mu$Set_Trap <- paste(Video_Analysis_Log_mu$Set, "_", Video_Analysis_Log_mu$Trap, sep = "")
Video_Analysis_Log_mu <- Video_Analysis_Log_mu[Video_Analysis_Log_mu$Set_Trap %in% Clips_vs_Records$Set_Trap, ]
Video_Analysis_Log_mu <- Video_Analysis_Log_mu[order(Video_Analysis_Log_mu$Set_Trap, Video_Analysis_Log_mu$FileName.y), ]

STCE_AF <- STCE_AF[STCE_AF$Set_Trap %in% Video_Analysis_Log_mu$Set_Trap, ]
Video_Analysis_Log_mu <- Video_Analysis_Log_mu[Video_Analysis_Log_mu$Set_Trap %in% STCE_AF$Set_Trap, ]
Video_Analysis_Log_mu <- Video_Analysis_Log_mu[Video_Analysis_Log_mu$FileName.x != "", ]

row.names(Video_Analysis_Log_mu) <- seq(1, nrow(Video_Analysis_Log_mu), 1)
row.names(STCE_AF) <- seq(1, nrow(STCE_AF), 1)

# -------------------------------------------

data <- cbind(Video_Analysis_Log_mu, STCE_AF)
head(data)
write.csv(data, file = "data_accel.csv")
save(data, file = "data_accel.RData")
# data <- data[data$Sounds != "0", ]
# data <- data[data$TrapMotion %in% c("0", "1"), ]
# data <- data[data$TrapMotion %in% c("0", "1", "6"), ]
data <- data[data$TrapMotion %in% c("0", "1", "2", "3", "6", "9"), ]
# data <- data[!(data$TrapMotion %in% c("4", "5")), ]
table(data$TrapMotion)
Sablefish_Survey_2013$lu_trap_motion

# -------------------------------------------------

Inputs <- paste("V", seq(1, 20, 1), sep = "")
ncol(AF)

for(Input in Inputs) {
	print(sd(data[, Input]))
	if(sd(data[, Input] > 0)) {
		data[, Inputs] <- data[, Input]/sd(data[, Input])
	}
}

# -----------------------------------
# unique set, trap combos
set_camera <- str_match(data$Filenames, "(.+)_GOPR.+")[, 2] 
data$set_camera <- set_camera
set_camera_u <- unique(set_camera)
set_camera_u

# -----------------------------------

Outputs_f <- vector()
for(Output in Outputs) {
	Outputs_f_o <- unique(data[, Output])
	for(Output_f_o in Outputs_f_o) {
		data[, paste(Output, Output_f_o, sep = "")] <- as.numeric(ifelse(data[, Output] == Output_f_o, 1, 0))
		print(paste(Output, " ", Output_f_o, sep = ""))
		Outputs_f <- c(Outputs_f, paste(Output, Output_f_o, sep = ""))
	}
	data[, Output] <- factor(data[, Output])
}

# formula for factors, and for regular
formula_f <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs, collapse = "+")))
formula <- as.formula(paste(paste(paste("factor(", Outputs, ")", sep = ""), collapse = "+"), "~", paste(Inputs, collapse = "+")))

# ------------------------------------------------------

results <- vector("list", N)
n_nodes <- ceiling(mean(c(length(Outputs_f), length(Inputs))))
n_layer <- 1
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

save(per_correct_resu, file = paste("per_correct_accel", paste(Outputs, collapse = ""), ".RData", sep = ""))



confusion_arnn
confusion_mode
confusion_nbay
confusion_rndf
