# ---------------------------------------------
# Output:
# - Loads training data output from Video_training_data.r
# - Subsets data so that Inputs, Outputs are non NA
# - Create formula for Outputs ~ Inputs
# - Train each classifier on complete training data set and save
# - Cross validation. For each i = 1,... N, randomly split the data into two pieces, one consisting of 2/3 of the Set/Trap combinations and 1/3 of the Set/Trap combinations. Train each classifier on 2/3 data, and test on 1/3, and store the result
# - Print and store the average percent correct from the cross validation
# ---------------------------------------------

# ---------------------------------------------
# - Loads training data output from Video_training_data.r
setwd(master_dir)
load(file = paste("data_train", year, ".RData", sep = ""))

print(paste("Outputs: ", Outputs, sep = ""))

# if less than 2 unique classes in Ouput, stop
if(as.integer(length(unique(as.character(na.omit(data_train[, Outputs]))))) <= 1) {
  stop("Need classifications to train on!")
}

# ---------------------------------------------
# - Subsets data so that Inputs, Outputs are non NA

# for given output, sets input and subsets data
require(stringr)
if(c("ImageClarity") %in% Outputs) {

  # sets inputs as; Princ1, Princ2,...
  Inputs <- na.omit(as.vector(str_match(as.vector(names(data_train)), "Princ.+")))
  Inputs

} else if (c("BioticMaterial" %in% Outputs)) {

  # sets inputs as; Areas1, Areas2,...
  Inputs <- na.omit(as.vector(str_match(names(data_train), "Areas.+")))
  Inputs

  # subsets to Clear videos
  data_train <- data_train[data_train$Filenames %in% Clear, ]

} else {

  # sets inputs as; Areas1, Areas2,...
  Inputs <- as.vector(na.omit(as.vector(str_match(names(data_train), "Areas.+"))))
  Inputs

  # subsets to Clear videos
  data_train <- data_train[data_train$Filenames %in% Clear, ]

  # subsets to no BioticMaterial, or Species_SSS = 1, but not both
  data_train <- data_train[
    is.na(data_train$Species_) == FALSE &
    is.na(data_train[, Outputs]) == FALSE &
    xor(data_train$Species_ == "0", data_train[, Outputs] == "1")
    , ]

}

# ---------------------------------------------
# - Subsets data so that Inputs, Outputs are non NA
data_train <- data_train[rowSums(is.na(data_train[, c(Inputs, Outputs)]) == TRUE) == 0, ]

print(paste("Training classifier on: ", nrow(data_train), sep = ""))

# ---------------------------------------------

# unique set, trap combos
require(stringr)
set_camera <- str_match(data_train$Filenames, "(.+)_GOPR.+")[, 2] 
data_train$set_camera <- set_camera
set_camera_u <- unique(set_camera)
set_camera_u

# ---------------------------------------------
# - Create formula for Outputs ~ Inputs

Outputs_f <- vector()
for(Output in Outputs) {
  Outputs_f_o <- unique(data_train[, Output])
  for(Output_f_o in Outputs_f_o) {
  data_train[, paste(Output, Output_f_o, sep = "")] <- as.numeric(ifelse(data_train[, Output] == Output_f_o, 1, 0))
  print(paste(Output, " ", Output_f_o, sep = ""))
  Outputs_f <- c(Outputs_f, paste(Output, Output_f_o, sep = ""))
  }
  data_train[, Output] <- factor(data_train[, Output])
}

# formula for factors, and for regular
formula_f <- as.formula(paste(paste(Outputs_f, collapse = "+"), "~", paste(Inputs, collapse = "+")))
formula <- as.formula(paste(paste(paste("factor(", Outputs, ")", sep = ""), collapse = "+"), "~", paste(Inputs, collapse = "+")))

# formula
# formula_f
# as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))

# ---------------------------------------------
# - Train each classifier on complete training data set and save

print(table(data_train[, Outputs]))

# artificial neural network
require(neuralnet)
setwd(master_dir)
arnn <- neuralnet(formula_f, rep = 1, stepmax = 10^6, data = data_train)
save(arnn, file = paste("arnn_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

# random forest
require(randomForest)
rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
  , data = data_train
  , replace = TRUE
  , strata = factor(rep(unique(data_train[, Outputs]), nrow(data_train)))
  )
print(rndf)
save(rndf, file = paste("rndf_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

# naive bayes
require(klaR)
nbay <- NaiveBayes(formula = formula, 
  data = data_train, 
  prior = rep(1/length(unique(data_train[, Outputs])), length(unique(data_train[, Outputs])))
  )
save(nbay, file = paste("nbay_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

# ---------------------------------------------
# - Cross validation. For each i = 1,... N, randomly split the data into two pieces, one consisting of 2/3 of the Set/Trap combinations and 1/3 of the Set/Trap combinations. Train each classifier on 2/3 data, and test on 1/3, and store the result

# unique set, trap combos
require(stringr)
set_camera <- str_match(data_train$Filenames, "(.+)_GOPR.+")[, 2] 
data_train$set_camera <- set_camera
set_camera_u <- unique(set_camera)
set_camera_u

# empty result list to fill
results <- vector("list", N)

# set parameters for artificial neural network
n_nodes <- ceiling(mean(c(length(Outputs_f), length(Inputs))))
n_layer <- 1

# start cross validation
print("Starting: cross validation")
i <- 1
# for(i in seq(1, N, 1)) {
results <- foreach(i = seq(1, N, 1)) %dopar% {

  print(paste("Cross validation: ", i, sep = ""))

  # randomly split data into 2/3, 1/3 unique set/trap combinations
  SC_sc <- sample(seq(1, length(set_camera_u), 1), size = ceiling(length(set_camera_u)*(2/3)))
  train <- seq(1, nrow(data_train), 1)[set_camera %in% set_camera_u[SC_sc]]
  # train <- sample(seq(1, nrow(data_train), 1), size = ceiling(nrow(data_train)*(2/3)))
  data_tra <- data_train[train, ]
  data_tes <- data_train[-train, ]

  tryCatch({

    # train artificial neural network on training data
    require(neuralnet)
    arnn <- neuralnet(formula_f, rep = 1, stepmax = 10^5, data = data_tra)
    # run classifier on test data, and store result
    arnn_result <- compute(x = arnn, data_tes[, Inputs])

    # random forest and naive bays
    rndf_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs))
    nbay_result <- matrix(NA, nrow = nrow(data_tes), ncol = length(Outputs))
    o <- 1
    for(o in seq(1, length(Outputs), 1)) {

      Output <- Outputs[o]
      Output

      # train naive bayes on training data
      require(klaR)
      nbay <- NaiveBayes(formula = formula, 
        data = data_tra, 
        prior = rep(1/length(unique(data_tra[, Outputs])), length(unique(data_tra[, Outputs])))
        )
      # run classifier on test data, and store result
      nbay_result[, o] <- paste(Output, as.vector(predict(object = nbay, newdata = data_tes[, Inputs])$class), sep = "")
      nbay_result

      # train random forest on training data
      require(randomForest)
      rndf <- randomForest(as.formula(paste("factor(", Output, ")", "~", paste(Inputs, collapse = "+")))
      , data = data_tra
      , replace = TRUE
      , strata = factor(rep(unique(data_tra[, Outputs]), nrow(data_tra)))
      )
      # run classifier on test data, and store result
      rndf_result[, o] <- paste(Output, as.vector(predict(object = rndf, newdata = data_tes)), sep = "")
      rndf_result

    }
    nbay_result <- as.data.frame(nbay_result)
    names(nbay_result) <- Outputs
    rndf_result <- as.data.frame(rndf_result)
    names(rndf_result) <- Outputs

    # train crude mode classifier on training data
    data_tra_c <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_tra[, Outputs_f])
    mode_table <- apply(data_tra_c, MARGIN = 2, FUN = function(x) names(which.max(table(x))))
    # run crude mode classifier on test data, and store result
    mode_result <- matrix(rep(mode_table, nrow(data_tes)), nrow = nrow(data_tes), byrow = TRUE)
    mode_result <- as.data.frame(mode_result)
    names(mode_result) <- names(mode_table)

    # write results for each classifier to result
    results[[i]][["arnn_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = arnn_result$net.result)
    results[[i]][["rndf_result"]] <- rndf_result
    results[[i]][["nbay_result"]] <- nbay_result
    results[[i]][["mode_result"]] <- mode_result
    results[[i]][["data_result"]] <- collapse(Outputs = Outputs, Outputs_f = Outputs_f, result = data_tes[, Outputs_f])

    results[[i]]

  },
  error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
  )
}
print("Finished: cross validation")

# index non null results as non_null. null results are as when the 2/3 training subset happens to have less than 2 unique classifications
non_null <- seq(1, N, 1)[unlist(lapply(1:N, FUN = function(i) is.null(results[[i]]) == FALSE))]
non_null

# subset results to non null results
results_nn <- vector("list", length(non_null))
for(i in 1:length(non_null)) {
  results_nn[[i]] <- results[[non_null[i]]]
}

# save results
print(paste("results", year, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(results, file = paste("results", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

# ---------------------------------------------
# - Print and store the average percent correct from the cross validation

# create empty list to fill with percent correct for each classifier
per_correct_resu <- list()
for(o in seq(1, length(Outputs), 1)) {

  Output <- Outputs[o]

  Outputs_f_o <- na.omit(str_match(Outputs_f, paste(Output, ".+", sep = "")))
  Outputs_f_o

    # calculate confusion matrix for each classifier
  confusion_arnn <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
  confusion_rndf <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
  confusion_nbay <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))
  confusion_mode <- matrix(0, nrow = nrow(Outputs_f_o), ncol = nrow(Outputs_f_o))

  # sets initial percent correct as 0
  per_correct_arnn <- 0
  per_correct_rndf <- 0
  per_correct_nbay <- 0
  per_correct_mode <- 0

  # creates empty vector for each classifier to fill with percent correct values
  per_correct_arnns <- vector()
  per_correct_rndfs <- vector()
  per_correct_nbays <- vector()
  per_correct_modes <- vector()

  # sums up confusion matrices across each iteraion i, for each classifier
  # calculates percent correct for the confusion matrix in each iteration for each classifier and stores it in vector
  for(i in 1:length(results_nn)){

    # confusion matrix for each classifier
    confusion_arnn <- confusion_arnn + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")
    confusion_rndf <- confusion_rndf + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")
    confusion_nbay <- confusion_nbay + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")
    confusion_mode <- confusion_mode + confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")

    # per_correct_arnn <- per_correct_arnn + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "arnn_result"))
    # per_correct_rndf <- per_correct_rndf + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "rndf_result"))
    # per_correct_nbay <- per_correct_nbay + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "nbay_result"))
    # per_correct_mode <- per_correct_mode + per_correct(confusion(i = i, Output = Outputs[o], results = results, var2 = "mode_result"))

    # percent correct for each classifier
    per_correct_arnns <- c(per_correct_arnns, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "arnn_result")))
    per_correct_rndfs <- c(per_correct_rndfs, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "rndf_result")))
    per_correct_nbays <- c(per_correct_nbays, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "nbay_result")))
    per_correct_modes <- c(per_correct_modes, per_correct(confusion(i = i, Output = Outputs[o], results = results_nn, var2 = "mode_result")))

  }

  # list containing percent correct for each iteration in each classifier
  per_correct_resu <- list(
    per_correct_arnns = per_correct_arnns, 
    per_correct_rndfs = per_correct_rndfs,
    per_correct_nbays = per_correct_nbays, 
    per_correct_modes = per_correct_modes
    )

  # print mean percent correct for each classifier
  print(Outputs[o])
  print(paste("arnn", " ", mean(per_correct_arnns), sep = ""))
  print(paste("rndf", " ", mean(per_correct_rndfs), sep = ""))
  print(paste("nbay", " ", mean(per_correct_nbays), sep = ""))
  print(paste("mode", " ", mean(per_correct_modes), sep = ""))
  print("")

}

# saves percent correct information
print(paste("per_correct_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))
save(per_correct_resu, file = paste("per_correct_video", year, paste(Outputs, collapse = ""), ".RData", sep = ""))

