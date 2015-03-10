setwd(master_dir)
# load(file = paste("data_video", year, ".RData", sep = ""))
load(file = paste("data_videoSTRS2013.RData", sep = ""))

# ---------------------------------------------

require(stringr)
if(c("ImageClarity") %in% Outputs) {
	Inputs <- na.omit(as.vector(str_match(as.vector(names(data)), "Video.+")))
} else if (c("IfBiotic" %in% Outputs)) {
	Inputs <- na.omit(as.vector(str_match(names(data), "Areas.+")))
	data <- data[data$Filenames %in% Clear, ]
}

# ---------------------------------------------

# unique set, trap combos
require(stringr)
set_camera <- str_match(data$Filenames, "(.+)_GOPR.+")[, 2] 
data$set_camera <- set_camera
set_camera_u <- unique(set_camera)
set_camera_u

# ---------------------------------------------

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

# ----------------------------------
# formula for factors, and for regular
formula_f <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs, collapse = "+")))
formula <- as.formula(paste(paste(paste("factor(", Outputs, ")", sep = ""), collapse = "+"), "~", paste(Inputs, collapse = "+")))

formula_f
formula

# ------------------------------------------------------
# artificial neural network
require(neuralnet)
setwd(master_dir)
arnn <- neuralnet(formula_f, rep = 1, stepmax = 10^6, data = data)
save(arnn, file = paste("arnn_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

require(randomForest)
rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
	, data = data
	, replace = TRUE
	, strata = factor(rep(unique(data[, Outputs]), nrow(data)))
	)
rndf
save(rndf, file = paste("rndf_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

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

	table(data_train[, Outputs])

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

				# # naive bayes
				# require(klaR)
				# nbay <- NaiveBayes(formula = formula, data = data_train, prior = rep(1/length(unique(data_train[, Outputs])), length(unique(data_train[, Outputs]))))
				# nbay_result[, o] <- paste(Output, as.vector(predict(object = nbay, newdata = data_test[, Inputs])$class), sep = "")

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

print(paste("per_correct_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(per_correct_resu, file = paste("per_correct_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

