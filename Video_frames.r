setwd(master_dir)

video_files = sort(list.files(video_dir))
video_files

video_file <- video_files[5]
video_file

Mean_frame <- function(video_file, video_dir, frame_dir, skip, Hgp, Wgp) {

	setwd(master_dir)

	print(video_file)

	require(stringr)
	base_file <- str_match(video_file, "(.+)\\..+4")[, 2]
	base_file
	
	require(rPython)
	python.assign("video_file", video_file)
	python.assign("base_file", base_file)
	python.assign("video_dir", video_dir)
	python.assign("frame_dir", frame_dir)
	python.assign("skip", skip)
	python.assign("Hgp", Hgp)
	python.assign("Wgp", Wgp)
	python.assign("data", 0)
	python.load("Remove_trap.py")
	python.load("Mean_frame.py")
	tryCatch({
		python.exec("
		  os.chdir(video_dir)
		  frame_o = Mean_frame( video_file, video_dir, skip, data, Hgp, Wgp)
		  os.chdir(frame_dir)
		  cv2.imwrite(base_file + '.jpg', frame_o)
		")
		},
		error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
		)
}

video_file <- "SKBO2014_S009T003_GOPR0434.MP4"
Mean_frame(video_file = video_file, video_dir = video_dir, frame_dir = frame_dir, skip = skip, Hgp = Hgp, Wgp = Wgp)

mclapply(video_files, 
	FUN = function(video_file) Mean_frame(video_file = video_file
		, video_dir = video_dir, frame_dir = frame_dir, skip = skip, Hgp = Hgp, Wgp = Wgp)
	, mc.cores = n_cores
	, mc.silent = FALSE
	, mc.preschedule = TRUE
	)

