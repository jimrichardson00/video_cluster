setwd("/home/jim/Desktop/video_cluster")

require(parallel)
require(neuralnet)
require(sqldf)
require(modeest)
require(stringr)
require(randomForest)
require(Hmisc)
require(reshape)
require(prettyR)
require(e1071)

collapse <- function(Outputs, Outputs_f, result) {

	result_collapse <- matrix(NA, ncol = length(Outputs), nrow = nrow(result))

	o <- 1
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

# load pca data
load("ccipca.RData")
video_files_cur <- ccipca[["video_files_cur"]]
prx <- ccipca[["prx"]]
RepFrames_cur <- ccipca[["RepFrames_cur"]]
prx <- as.data.frame(prx)
for(name in names(prx)){
	prx[, name] <- as.numeric(prx[, name])
}
prx$video_files <- video_files_cur

# load video log data
Sablefish_Survey_2013 <- mdb.get("2013_Sablefish_Survey.mdb")
names(Sablefish_Survey_2013)
Sablefish_Survey_2013$lu_substrate
Sablefish_Survey_2013$lu_impact_evidence
Video_Analysis_Log <- Sablefish_Survey_2013$data
Video_Analysis_Log <- Video_Analysis_Log[order(Video_Analysis_Log$TransectName, Video_Analysis_Log$FileName), ]
Video_Analysis_Log$Comment2 <- ifelse(is.na(str_match(Video_Analysis_Log$Comment, "obscured")) == TRUE, 0, 1)
Video_Analysis_Log_u <- unique(Video_Analysis_Log[, c("TransectName", "FileName", Outputs)])
Video_Analysis_Log_u <- Video_Analysis_Log_u[order(Video_Analysis_Log_u$TransectName, Video_Analysis_Log_u$FileName), ]

write.csv(Video_Analysis_Log_u, "Video_Analysis_Log_u.csv")
write.csv(Video_Analysis_Log, "Video_Analysis_Log.csv")

# set video data as character
for(name in names(Video_Analysis_Log_u)){
	Video_Analysis_Log_u[, name] <- as.character(Video_Analysis_Log_u[, name])
}

# join video log data to video data
Video_Analysis_Log_u$Set <- str_match(Video_Analysis_Log_u$TransectName, "set(.+)camera(.+)")[, 2]
Video_Analysis_Log_u$Trap <- str_match(Video_Analysis_Log_u$TransectName, "trap(.+)")[, 2]
Video_Analysis_Log_u$video_files <- paste("STRS2013", "_", "S", Video_Analysis_Log_u$Set,
                                         "T", Video_Analysis_Log_u$Trap, "_", Video_Analysis_Log_u$FileName, sep = "")
Video_Analysis_Log_u[is.na(Video_Analysis_Log_u) == TRUE] <- 'NA'
Video_Analysis_Log_u[Video_Analysis_Log_u == ""] <- 'NA'

# merged data set
data <- merge(x = prx, y = Video_Analysis_Log_u, by.x = "video_files", by.y = "video_files")

nrow(data)
data


# unique set, trap combos
set_camera <- str_match(data$video_files, "(.+)_GOPR.+")[, 2] 
data$set_camera <- set_camera
set_camera_u <- unique(set_camera)
set_camera_u

# set outputs and inputs
Outputs <- c("ImageQualityID", "OnBottom", "Complexity", "ImpactEvidence", "DominantSubstrate", "Comment2")
Outputs <- c("DominantSubstrate")
Inputs <- paste("V", seq(1, 20, 1), sep = "")

# set new output list with 0, 1 true false for each category within each variable
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
formula <- as.formula(paste(paste(Outputs, collapse = "+"), "~", paste(Inputs, collapse = "+")))

N <- 1000

i <- 1
results <- vector("list", N)
n_nodes <- ceiling(mean(c(length(Outputs_f), length(Inputs))))
n_nodes
n_layer <- 5
Outputs
i <- 1
for(i in seq(1, N, 1)) {

	print(i)

	SC_sc <- sample(seq(1, length(set_camera_u), 1), size = ceiling(length(set_camera_u)*(2/3)))
	train <- seq(1, nrow(data), 1)[set_camera %in% set_camera_u[SC_sc]]

	data_train <- data[train, ]
	data_test <- data[-train, ]

	# artificial neural network
	arnn <- neuralnet(formula_f, hidden = rep(n_nodes, n_layer), threshold = 0.01, stepmax = 10^6, data = data_train)
	arnn_result <- compute(x = arnn, data_test[, Inputs])

	# random forest
	rndf_result <- matrix(NA, nrow = nrow(data_test), ncol = length(Outputs))
	nbay_result <- matrix(NA, nrow = nrow(data_test), ncol = length(Outputs))
	for(o in seq(1, length(Outputs), 1)) {
		tryCatch({
			Output <- Outputs[o]

			nbay <- naiveBayes(data_train[, Inputs], data_train[, Output]) 
			nbay_result[, o] <- paste(Output, as.vector(predict(object = nbay, newdata = data_test[, Inputs])), sep = "")

			rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+"))), data = data_train)
			rndf_result[, o] <- paste(Output, as.vector(predict(object = rndf, newdata = data_test)), sep = "")

			},
			error = function(e) {cat("ERROR :",conditionMessage(e), "\n")})
	}
	nbay_result <- as.data.frame(nbay_result)
	names(nbay_result) <- Outputs
	nbay_result
	rndf_result <- as.data.frame(rndf_result)
	names(rndf_result) <- Outputs
	rndf_result

	# mode value
	data_train_c <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_train[, Outputs_f])
	mode_table <- apply(data_train_c, MARGIN = 2, FUN = function(x) names(which.max(table(x))))
	mode_result <- matrix(rep(mode_table, nrow(data_test)), nrow = nrow(data_test), byrow = TRUE)
	mode_result <- as.data.frame(mode_result)
	names(mode_result) <- names(mode_table)
	mode_result

	results[[i]][["arnn_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = arnn_result$net.result)
	results[[i]][["rndf_result"]] <- rndf_result
	results[[i]][["nbay_result"]] <- nbay_result
	results[[i]][["mode_result"]] <- mode_result
	results[[i]][["data_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_test[, Outputs_f])

}

o <- 4
Outputs[o]
confusion(i = i, Output = Outputs[o], results = results, var2 = "arnn_result")
results[[1]][["arnn_result"]]['DominantSubstrate']
results[[1]][["data_result"]]['DominantSubstrate']

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

	for(i in 1:N){
		confusion_arnn <- confusion_arnn + confusion(i = i, Output = Outputs[o], results = results, var2 = "arnn_result")
		confusion_rndf <- confusion_rndf + confusion(i = i, Output = Outputs[o], results = results, var2 = "rndf_result")
		confusion_nbay <- confusion_nbay + confusion(i = i, Output = Outputs[o], results = results, var2 = "nbay_result")
		confusion_mode <- confusion_mode + confusion(i = i, Output = Outputs[o], results = results, var2 = "mode_result")

		per_correct_arnn <- per_correct_arnn + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "arnn_result"))
		per_correct_rndf <- per_correct_rndf + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "rndf_result"))
		per_correct_nbay <- per_correct_nbay + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "nbay_result"))
		per_correct_mode <- per_correct_mode + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "mode_result"))
	}

	print(Outputs[o])

	# print(paste("arnn", " ", per_correct(confusion_arnn)))
	# print(paste("rndf", " ", per_correct(confusion_rndf)))
	# print(paste("mode", " ", per_correct(confusion_mode)))

	print(paste("arnn", " ", per_correct_arnn/N, sep = ""))
	print(paste("rndf", " ", per_correct_rndf/N, sep = ""))
	print(paste("nbay", " ", per_correct_nbay/N, sep = ""))
	print(paste("mode", " ", per_correct_mode/N, sep = ""))
	print("")

	print(paste("arnn"))
	confusion_arnn
	print(paste("rndf"))
	confusion_rndf
	print(paste("mode"))
	confusion_mode
	print("")

}


data

o
Outputs[6]
confusion(i = i, OutputsOutput = Outputs[6], results = results, var2 = "rndf_result")
confusion(i = i, Output = Outputs[4], results = results, var2 = "mode_result")
length(results)

data$ImpactEvidence
confusion_rndf
confusion_arnn
per_correct(confusion_arnn)

confusion_rndf
per_correct(confusion_rndf)

confusion_mode
per_correct(confusion_mode)

# ---------------------------------------------------------
# ---------------------------------------------------------

N = 100


v <- 5
v <- 6

Outputs[v]
length(Outputs)
for(v in c(3, 4, 5, 6)){

	SC_sc <- sample(seq(1, length(set_camera_u), 1), size = ceiling(length(set_camera_u)*(2/3)))
	train <- seq(1, nrow(data), 1)[set_camera %in% set_camera_u[SC_sc]]
	train

	data_train <- data[train, ]
	data_test <- data[-train, ]

	rf <- randomForest(as.formula(paste(Outputs[v], "~", paste(Inputs, collapse = "+"))), data = data_train)

	print(Outputs[v])
	names(rf)

paste(Outputs[v], as.vector(predict(object = rf, newdata = data_test)), sep = "")

	print(rf$confusion)
	per_correct(rf$confusion)
	diag(rf$confusion)
	sum(rf$confusion)
	length(Out)
	print("")

}



names(rf)

Outputs[3]

names(Video_Analysis_Log_u)

rf$confusion

rf$test

print(rf) # view results 
importance(rf) # importance of each predictor
varImpPlot(rf)

data

partialPlot(rf)
MDSPlot(rf)

?MDSPlot

# ------------------------------------------

library(Hmisc)

data <- mdb.get("SablefishCameraAnalysis.mdb")

names(data)
data$C_Substrate

data2 <- mdb.get("2013_Sablefish_Survey.mdb")

names(data2)

data2$lu_complexity
data2$lu_species_code

data_s <- data2$data
nrow(unique(data.frame(A = data_s$TransectName, )))

nrow(data2$data)

nrow(data)
data2