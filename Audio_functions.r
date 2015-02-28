audio_file <- "STRS2013_S004T014_GOPR0612.wav"

Audio_features <- function(audio_file, buffer) {

	print(paste("Starting: ", audio_file, sep = ""))

	setwd(audio_dir)

	require(tuneR)
  	audio <- readWave(audio_file)
  	f <- audio@samp.rate
  	duration <- floor(length(audio@left)/f)
  	duration

  	# audio <- readWave(audio_file, from = 1, to = duration - 1, units = "seconds")
  	audio <- readWave(audio_file, from = 1, to = 11, units = "seconds")
  	f <- audio@samp.rate

  	require(seewave)
	spec <- spec(audio, plot = FALSE, f = f, norm = FALSE)

	tryCatch({
		pgram <- periodogram(mono(audio, which = "both"))
		},
		error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
		)

	signal <- audio@left + audio@right

	# zcr
	zcr <- zcr(audio, wl = NULL)

	# spectral centriod
	cent <- specprop(spec)$cent

	# roughness
	roughness <- roughness(signal)

	# rms
	rms <- rms(signal)

	# q25
	Q25 <- specprop(spec)$Q25

	# q75
	Q75 <- specprop(spec)$Q75

	# iqr
	IQR <- specprop(spec)$IQR

	# rugo
	rugo <- rugo(signal/max(signal))
  
	# entropy
	H <- H(audio)
	H

	# # variance
	variance <- pgram@variance
	variance

	# # energy
	energy <- pgram@energy
	energy

	# melfcc
	melfcc <- melfcc(mono(audio, which = "both"))
	melfcc <- as.vector(apply(melfcc, MARGIN = 2, FUN = mean))

	features <- c(zcr, cent, roughness, rms, Q25, Q75, IQR, rugo, H, variance, energy, melfcc)
	names(features) <- c("zcr", "cent", "roughness", "rms", "Q25", "Q75", "IQR", "rugo", "H", "variance", "energy", paste("melfcc", 1:length(melfcc), sep = ""))

	print(paste("Finished: ", audio_file, sep = ""))

	return(as.vector(features))
}

sampleS <- function(signal, p = 0.1) {
	n <- length(signal)
	np <- floor(n*p)
	return(signal[seq(1, n, length = np)])
}

sampleL <- function(signal, l) {
	n <- length(signal)
	signal[seq(1, n, length = l)]
}

sampleR <- function(signal, p) {
	n <- length(signal)
	np <- round(n*p)
	sample(signal, size = np, replace = FALSE)
}

Plot_SpecGraph <- function(audio_file) {
	print(paste("Starting: ", audio_file, sep = ""))
	setwd(audio_dir)
	require(tuneR)
	audio <- readWave(audio_file)
	Fs <- audio@samp.rate
	require(signal)
	spec <- specgram(x = audio@right + audio@left, Fs = Fs)
	plot(spec, main = audio_file)
}

Extract_SpecGraph <- function(audio_file) {
	print(paste("Starting: ", audio_file, sep = ""))
	setwd(audio_dir)
	# require(tuneR)
	audio <- readWave(audio_file)
	Fs <- audio@samp.rate
	require(signal)
	spec <- specgram(x = audio@right + audio@left, Fs = Fs)
	S <- t(abs(spec$S))
	t <- spec$t
	f <- spec$f
	ncolS <- ncol(S)
	ncolS
	nrowS <- nrow(S)
	nrowS
	floor(max(t))
	Sis <- list()
	for(ti in seq(1, floor(max(t)), 1)) {
		Si <- S[floor(sort(rep(t, ncolS))) == ti - 1]
		Sis[[ti]] <- Si
	}
	names(Sis) <- as.character(seq(1, floor(max(t)), 1))
	print(names(Sis))
	print(head(Sis[["1"]]))
	print(paste("Finished: ", audio_file, sep = ""))
	return(Sis)
}

Extract_Spectrum <- function(audio_file) {
	print(paste("Starting: ", audio_file, sep = ""))
	setwd(audio_dir)
	require(tuneR)
	audio <- readWave(audio_file)
	Fs <- audio@samp.rate
	signal <- audio@right + audio@left
	signal <- signal[seq(Fs, length(signal) - Fs, 1)]
	require(stats)
	Spectrum <- spectrum(x = signal, plot = FALSE)
	smth <- smooth.spline(Spectrum$freq, log(Spectrum$spec))

	# plot(sampleS(Spectrum$freq, 0.1), sampleS(log(Spectrum$spec), 0.1), type = "l", main = audio_file)
	# lines(smth$x, smth$y, col = "red")
	# points(sampleL(smth$x, 1000), sampleL(smth$y, 1000), pch = 16, col = "red")

	print(head(smth$y))
	print(paste("Finished: ", audio_file, sep = ""))

	return(sampleL(smth$y, l = 1000))
}

Plot_Spectrum <- function(audio_file) {
	setwd(audio_dir)
	require(tuneR)
	audio <- readWave(audio_file)
	Fs <- audio@samp.rate
	require(stats)
	signal <- audio@right + audio@left
	signal <- signal[seq(Fs, length(signal) - Fs, 1)]
	Spectrum <- spectrum(x = signal, plot = FALSE)

	smth <- smooth.spline(Spectrum$freq, log(Spectrum$spec))

	xs <- sampleL(Spectrum$freq, 5000)
	ys <- sampleL(log(Spectrum$spec), 5000)

	x <- sampleL(smth$x, 1000)
	y <- sampleL(smth$y, 1000)

	plot(xs,ys, type = "l", main = audio_file, ylim = c(0, 20), xlim = c(0, 0.5))
	lines(x, y, col = "red", pch = 16)
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


